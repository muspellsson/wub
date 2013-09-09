# Session - handle Session vars stored in a db, indexed by a cookie.
#
# This is to be run in the main thread, and to amend the -session var in
# a request, to be passed along with request to Workers.
#
# ARCHITECTURE:
#
# Cookies shouldn't be sent with a path of /, because it screws up caching
# Session maintains a key in cookie which is sent within a constrained path.
# (a) how can a cookie within a constrained path be used
# everywhere on a site when it's only sent for a subpath of the site - use a
# web-bug technique - load some file from session domain on every page, don't cache
# that subdomain.  Use Wub to associate the cookie with the whole session.
# (b) how can a cookie within a sub-path impact on the whole page as seen by a
# client?  Javascript.  Send javascript from the web bug.
#
# IMPLEMENTATION:
# Sessions are records in a database which are stored in the corovar scope (uplevel #1)
# the cookie maps to a defined coroutine which is maintained by Session, and in which all
# request processing occurs.

# If the session subdict changes during processing of the request, changes will
# be rewritten to the database.
#
# Examples:
#
# 1) Fetch a session
#
#	set req [Session fetch $req] ;# fetch session specified by cookie
#	set session [dict get $req -session]	;# this is the session dict
#	if {[dict exists $session _key]} {
#		# this is an existing session
#		puts "[dict get $session _key] is the session key"
#	} else {
#		# this is a brand new session (as no key has been assigned)
#	}
#
# 2) Modify/use session state
#
# FIRST:
#	dict set req -session somevar $value	;# set a session variable
#
# THEN:
#	dict with req -session {
#		... modify existing variables - only works on existing sessions
#		# risky unless you can control the contents of the dict,
#		# as any element in dict *will* overwrite other vars.
#	}
# OR:
#	Session with req {
#		... modify existing variables
#	}
#
# FINALLY:
#	set rsp [Session store $req]	;# session doesn't exist until stored
#	return $rsp			;# the rsp will have required cookies
#
# 3) Remove a session from the database
#
#	set rsp [Session remove $req]
#	return $rsp

package require Debug
Debug define session 10
package require OO
package require md5

package provide Session 4.0
set ::API(Session) {
    {
	Session manager
    }
    cookie {session cookie name (default "session")}
    cpath {session cookie path (default "/" - this default is a bad idea)}
    db {open tdbc database}
    file {session database (default session.db)}
    schema {session table schema}
    schemafile {file containing session table schema}
}

class create ::Session {
    # id - the current session id
    classmethod id {} {
	namespace tail [info coroutine]
    }

    # Session variable - map a session variable into local scope
    classmethod variable {args} {
	Debug.session {Session variable $args}
	if {[llength $args] == 1} {
	    set n [lindex $args 0]
	    uplevel 1 [list upvar #1 $n $n]
	    upvar #1 $n $n
	    if {[info exists $n]} {
		puts stderr "$n session var exists: [set $n]"
	    } else {
		puts stderr "$n session var does not exist"
	    }
	} else {
	    foreach {n v} $args {
		uplevel 1 [list upvar #1 $n $n]
		uplevel 1 [list set $n $v]
	    }
	}
    }

    # active - the activity of active sessions
    method active {} {
	variable active; array get active
    }

    # close - a session within this Session
    classmethod close {} {	    # close from within a session
	uplevel #1 my close [info coroutine]
    }
    method close {session} {
	# close the named session
	::variable handler
	unset handler($session)
    }

    # /close - Direct Domain closure of session
    method /close {r} {
	variable ::cookie
	set id [dict get? [Cookies Fetch? $r -name $cookie] -value]
	if {$id ne ""} {
	    # we are inside the subject domain, so can use their cookie
	    set id [dict get [Cookies Fetch $r -name $cookie] -value]
	    set coro [namespace current]::Coros::$id	;# remember session coro name
	    my close $coro
	} else {
	    error "No Such Session"
	}
    }

    # idle - return list of sessions idle longer than the proffered time
    method idle {args} {
	if {![llength $args]} {
	    set args {1 day}
	}

	variable active;
	set now [clock seconds]
	set idle {}
	foreach {session when} [lsort -integer -stride 2 -index 1 [array get active]] {
	    if {[clock add $when {*}$args] > $now} {
		lappend idle $session
	    }
	}
	return $idle
    }

    # close_idle - close sessions which have been idle for more than a given time
    method close_idle {args} {
	foreach session [my idle {*}$args] {
	    my close $session
	}
    }

    # variables - the set of session variables
    classmethod variables {session} {	# access from within a session
	uplevel #1 my close [info coroutine]
    }
    method variables {session} {
	::variable variables; return $variables($session)
    }

    # varmod - record and return all session variable modifications
    method varmod {args} {
	::variable varmod
	if {![llength $args]} {
	    set result {}
	    if {[info exists varmod([info coroutine])]} {
		Debug.session {varmod summary [info coroutine] $varmod([info coroutine])}
		set write {}; set unset {}
		dict with varmod([info coroutine]) {}
		unset varmod([info coroutine])
		set result [list [dict keys $write] [dict keys $unset]]
	    }
	    Debug.session {varmod summary [info coroutine] $result}
	    return $result
	}

	Debug.session {varmod [info coroutine] $args}
	puts stderr "VARMOD [info coroutine] [info frame -1]/[info frame -2]/[info frame -3]"
	lassign $args name1 name2 op
	switch -- $op {
	    write {
		dict set varmod([info coroutine]) write $name1 1
		catch {dict unset varmod([info coroutine]) unset $name1}
	    }
	    unset {
		dict set varmod([info coroutine]) unset $name1 1
		catch {dict unset varmod([info coroutine]) write $name1}
	    }
	}
    }

    # corodead - the coroutine has died, clean up after it.
    method corodead {coro args} {
	::variable variables; catch {unset variables($coro)}
	::variable active; catch {unset active($coro)}
	::variable counter; catch {unset counter($coro)}
    }

    # self - for the shim
    method self {} {
	return [self]
    }

    # shim - coroutine code which indirects to the domain handler, providing a place to store
    # session vars etc.
    # The handler is run in an apply to keep variable scope #1 pristine
    # the [my] command will invoke in this Session instance.
    method shim {args} {
	::apply [list {} {
	    ::variable fetcher	;# db prep'd statement to fetch session vars
	    ::variable updater	;# db prep'd statement to update fields
	    ::variable nuller 	;# db prep'd statement to null fields

	    ::variable active	;# introspection - last session access
	    ::variable counter	;# introspection - count of session accesses

	    ::variable handler	;# request handlers by coroutine
	    Debug.session {shim $handler([info coroutine]) START}
	    set id [namespace tail [info coroutine]]
	    set r {}
	    while {[info exists handler([info coroutine])]} {
		set r [::yieldm $r]
		if {![llength $r]} break
		set r [lindex $r 0]
		Debug.session {[info coroutine] got ($r)}

		set active([info coroutine]) [clock seconds]
		incr counter([info coroutine])

		# fetch session variables by key
		# do it only when we've got a real request
		::variable variables	;# introspect session var names
		if {![info exists variables([info coroutine])]} {
		    set vars [lindex [$fetcher allrows -as dicts [list cookie $id]] 0]
		    ::variable fields		;# names of all known session variables
		    Debug.session {shim $handler([info coroutine]) VARS ($fields) fetched ($vars)}
		    foreach n $fields {
			Debug.session {shim $handler([info coroutine]) var $n}
			catch {uplevel #1 [list ::trace remove variable $n {write unset} [list [my self] varmod]]}
			if {[dict exists $vars $n]} {
			    Debug.session {shim var assigning $n<-'[dict get $vars $n]'}
			    uplevel #1 [list set $n [dict get $vars $n]]
			} else {
			    if {[catch {uplevel #1 [list unset $n]} e eo]} {
				Debug.session {shim var unset $n err '$e' ($eo)}
			    }
			}
			uplevel #1 [list ::trace add variable $n {write unset} [list [my self] varmod]]
			lappend variables([info coroutine]) $n
		    }
		}

		# handle the request - if handler disappears, we're done
		set r [uplevel 1 [list $handler([info coroutine]) do $r]]
		
		# write back session variable changes
		lassign [my varmod] changed null
		
		foreach field $null {
		    $nuller($field) allrows [list cookie $id]	;# prepared db command nulls field
		}
		foreach field $changed {
		    $updater($field) allrows [list value [uplevel #1 [list set $field]] cookie $id];# prepared db command sets field
		}
	    }
	} [namespace current]]
    }

    # do - perform the action
    # we get URLs from both our Direct Domain (if it's mounted)
    method do {r} {
	# calculate the suffix of the URL relative to $mount
        variable mount
	if {[info exists mount]} {
	    # our Direct Domain is mounted
	    lassign [Url urlsuffix $r $mount] result r suffix path
	    if {$result} {
		Debug.session {passthrough to Direct for [self] $result suffix:$suffix path:$path}
		return [next $r]	;# the URL is in our Session domain, fall through to Direct
	    }
	}

	# the URL is (presumably) in one of the handled subdomains
	::variable cookie

	# fetch or create a cookie session identifier
	Debug.session {session cookie $cookie: [Cookies Fetch? $r -name $cookie] / ([dict get $r -cookies])}
	set id [Cookies Fetch? $r -name $cookie]
	if {$id eq ""} {
	    # There is no session cookie - create a new session, id, and cookie
	    Debug.session {create new session}

	    # create new session id
	    ::variable uniq; set id [::md5::md5 -hex [self][incr uniq][clock microseconds]]
	    variable inserter; $inserter allrows [list cookie $id]	;# add new session record

	    # create the cookie
	    ::variable cpath; ::variable expires; ::variable cookie_args;
	    set r [Cookies Add $r -path $cpath -expires $expires {*}$cookie_args -name $cookie -value $id]
	    Debug.session {new session: $id}
	} else {
	    # We have the session cookie
	    set id [dict get [Cookies Fetch $r -name $cookie] -value]
	    Debug.session {fetched session: $id}
	}

	# find active session with $id
	set coro [namespace current]::Coros::$id	;# remember session coro name
	if {![llength [info commands $coro]]} {
	    # we don't have an active session for this id - create one
	    variable handler; variable handlers
	    set handler($coro) $handlers([dict get $r -section])	;# get handler
	    Debug.session {create coro: $coro to handle $handler($coro) for session $id}
	    ::coroutine $coro [self] shim	;# create coro shim with handler
	    trace add command $coro delete [list [self] corodead]
	}

	# call the handler shim to process the request
	tailcall $coro $r
    }
    
    # new - create a Domain supervised by this Session manager
    # called when the domain is created by Nub, which thinks this Session instance is a class
    method new {domain args} {
	set mount [dict get $args mount]
	package require $domain
	::variable handlers
	set handlers($mount) [namespace eval ::Domains::$mount [list $domain new {*}$args]]
	return [self]
    }

    # create - create a named Domain supervised by this Session manager
    # called when the domain is created by Nub, which thinks this Session instance is a class
    method create {name args} {
	Debug.session {constructing handler name:'$name' $args}
	set mount [dict get $args mount]
	set domain [dict get $args domain]
	package require $domain
	::variable handlers
	set handlers($name) [$domain create $name {*}$args]
	return [self]
    }

    method / {r} {
	error "Can't get to here"
    }

    superclass Direct
    constructor {args} {
	Debug.session {constructing [self] $args}
	::variable tdbc sqlite3		;# TDBC backend
	::variable db ""		;# already open db
	::variable file ""		;# or db file
	::variable tdbc_opts {}		;# ::tdbc::connection creation options
	::variable schemafile ""	;# file containing schema
	::variable schema {}		;# schema for empty dbs
	
	::variable cookie "session"	;# session cookie name
	::variable cpath "/"		;# session cookie path - this default is a bad idea.
	::variable expires "next month"	;# how long does this cookie live?
	::variable cookie_args {}	;# extra args for cookie creation

	::variable {*}[Site var? Session]	;# allow .config file to modify defaults
	::variable {*}$args
	next {*}$args

	::variable handlers		;# handlers created by this session manager
	array set handlers {}
	::variable active		;# activity time per coro
	array set active {}
	::variable variables		;# session variables per coro
	array set variables {}
	::variable varmod		;# record session var mods per coro
	array set varmod {}

	# create the local namespace within which all coros will be created
	namespace eval [namespace current]::Coros {}

	# set up the DB table
	package require tdbc
	package require tdbc::$tdbc
	if {$db eq ""} {
	    if {$file eq ""} {
		error "Must provide a db file or an open db"
	    } else {
		set ons [namespace current]
		Debug.session {creating db: tdbc::${tdbc}::connection create ${ons}::dbI $file $tdbc_opts}
		file mkdir [file dirname $file]
		tdbc::${tdbc}::connection create ${ons}::dbI $file {*}$tdbc_opts
		oo::objdefine [self] forward db ${ons}::dbI	;# make a db command alias
	    }
	} else {
	    Debug.session {provided db: '$db'}
	    oo::objdefine [self] forward db {*}$db
	}

	if {[my db tables] eq ""} {
	    # we don't have any tables - apply schema
	    if {$schema eq "" && $schemafile ne ""} {
		set fd [open $schemafile r]
		set schema [read $fd]
		close $fd
	    }
	    if {$schema eq ""} {
		error "Must provide a schema,schemafile or an initialized db"
	    } else {
		Debug.session {schema: $schema}
		my db allrows $schema
	    }
	}

	if {$cookie ni [my db tables]} {
	    error "Session requires a table named '$cookie', named for the session cookie"
	}

	::variable fetcher
	if {![info exists fetcher]} {
	    set fetcher [my db prepare "SELECT * FROM $cookie WHERE $cookie == :cookie"]
	}
	::variable inserter
	if {![info exists inserter]} {
	    set inserter [my db prepare "INSERT INTO $cookie ($cookie) VALUES (:cookie)"]
	}
	# prepare some sql statemtents to NULL and UPDATE session vars
	::variable nuller
	::variable updater
	::variable fields
	foreach {field .} [my db columns $cookie] {
	    set nuller($field) [my db prepare "UPDATE $cookie SET $field = NULL WHERE $cookie = :cookie"]
	    set updater($field) [my db prepare "UPDATE $cookie SET $field = :value WHERE $cookie = :cookie"]
	    lappend fields $field
	}
	Debug.session {session provides vars '$fields'}
	package provide [namespace tail [self]] 1.0
    }
}

if {0} {
    # this goes in local.tcl
    namespace eval ::TestSession {
	proc / {r} {
	    Debug.session {TestSession running in [info coroutine]}
	    Session variable counter
	    if {[info exists counter]} {
		Debug.session {counter exists: $counter}
	    } else {
		Debug.session {counter does not exist}
	    }
	    incr counter
	    return [Http NoCache [Http Ok $r [<p> "COUNT $counter"]]]
	}
    }

    # this goes in site.config
    /session/mgr/ {
	domain {Session test_sm}
	cpath /session/
	file test_session.db
	schema {
	    PRAGMA foreign_keys = on;
	    PRAGMA synchronous=OFF;
	    DROP TABLE IF EXISTS cookie;
	    CREATE TABLE session (
				  session TEXT PRIMARY KEY,
				  counter INTEGER
				  );
	    CREATE INDEX session_index ON session(session);
	}
    }

    /session/ -session test_sm {
	domain Direct
	namespace ::TestSession
    }

    /session/code {
	# note, not under test_sm session manager, but is under domain, so will get the cookie
    }
}
