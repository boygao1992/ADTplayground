/* globals exports, setTimeout */
"use strict"

var AVar = function () {

  // double-linked list
  function MutableQueue() {
    this.head = null
    this.last = null
    this.size = 0
  }

  function MutableCell( queue, value ) {
    this.queue = queue
    this.value = value
    this.next = null
    this.prev = null
  }

  function AVar( value ) {
    this.draining = false
    this.error = null
    this.value = value
    this.takes = new MutableQueue()
    this.reads = new MutableQueue()
    this.puts = new MutableQueue()
  }

  var EMPTY = {}

  function runEff( eff ) {
    try {
      eff()
    } catch ( error ) {
      setTimeout( function () {
        throw error
      }, 0 )
    }
  }

  function putLast( queue, value ) {
    var cell = new MutableCell( queue, value )
    switch ( queue.size ) {
      case 0:
        queue.head = cell
        break
      case 1:
        cell.prev = queue.head
        queue.head.next = cell
        queue.last = cell
        break
      default:
        cell.prev = queue.last
        queue.last.next = cell
        queue.last = cell
    }
    queue.size += 1
    return cell
  }

  function takeLast( queue ) {
    var cell
    switch ( queue.size ) {
      case 0:
        return null
      case 1:
        cell = queue.head
        queue.head = null
        break
      case 2:
        cell = queue.last
        queue.head.next = null
        queue.last = null
        break
      default:
        cell = queue.last
        queue.last = queue.last.prev
        queue.last.next = null
    }
    queue.size -= 1
    cell.queue = null
    cell.prev = null
    return cell.value
  }

  function takeHead( queue ) {
    var cell
    switch ( queue.size ) {
      case 0:
        return null
      case 1:
        cell = queue.head
        queue.head = null
        break
      case 2:
        cell = queue.head
        queue.head = queue.last
        queue.head.prev = null
        queue.last = null
        break
      default:
        cell = queue.head
        queue.head = queue.head.next
        queue.head.prev = null
    }
    queue.size -= 1
    cell.queue = null
    cell.next = null
    return cell.value
  }

  function deleteCell( cell ) {
    if ( cell.queue == null ) {
      return
    }

    if ( cell.queue.last == cell ) {
      takeLast( cell.queue )
      return
    }

    if ( cell.queue.head == cell ) {
      takeHead( cell.queue )
      return
    }

    if ( cell.prev ) {
      cell.prev.next = cell.next
    }

    if ( cell.next ) {
      cell.next.prev = cell.prev
    }

    cell.queue.size -= 1
    cell.queue = null
    cell.value = null
    cell.prev = null
    cell.next = null
  }

  function drainVar( util, avar ) {
    /* Possible States
      0. killed: avar.error !== null
      1. blocked on Put
      2. blocked on Read/Take
     */
    /* Possible Events
      0. Put: avar.value !== EMPTY
      1. Read/Take: avar.value === EMPTY
     */
    if ( avar.draining ) { // runEff error handling is asynchronous, thus drainVar may get called multiple times in the same pass
      return
    }

    avar.draining = true

    var ps = avar.puts
    var ts = avar.takes
    var rs = avar.reads
    var p, r, t, value, rsize

    while ( true ) {
      p = null
      r = null
      t = null
      value = avar.value
      rsize = rs.size

      if ( avar.error !== null ) { // avar is killed
        value = util.left( avar.error ) // Left side in Either, to report Error to all callback functions in queues

        while ( ( p = takeHead( ps ) ) ) {
          runEff( p.cb( value ) )
        }
        while ( ( r = takeHead( rs ) ) ) {
          runEff( r( value ) )
        }
        while ( ( t = takeHead( ts ) ) ) {
          runEff( t( value ) )
        }
        break
      }

      if ( value == EMPTY && ( ( p = takeHead( ps ) ) ) ) {
        // if current Event is not Put, then pop one queued Put Event from Put queue
        avar.value = p.value
        value = p.value
      }

      if ( value !== EMPTY ) {
        // either current Event is Put which carries a value
        // or Put queue is not empty
        t = takeHead( ts )
        while ( ( rsize -= 1 ) && ( r = takeHead( rs ) ) ) {
          runEff( r( util.right( value ) ) )
        }
        if ( t !== null ) {
          avar.value = EMPTY
          runEff( t( util.right( value ) ) )
        }
      }
      // otherwise, Put queue is empty

      if ( p !== null ) {
        // if the value is from Put queue, evoke its callback function
        runEff( p.cb( util.right( void 0 ) ) )
      }

      if ( avar.value === EMPTY && ps.size === 0 || // block on Read/Take
        avar.value !== EMPTY && ts.size === 0 ) // block on Put
      {
        break
      }
    }

    avar.draining = false

  }

  AVar.EMPTY = EMPTY
  AVar.putLast = putLast
  AVar.takeLast = takeLast
  AVar.takeHead = takeHead
  AVar.deleteCell = deleteCell
  AVar.drainVar = drainVar
  return AVar
}()

exports.empty = function () {
  return new AVar( AVar.EMPTY )
}

exports._newVar = function ( value ) {
  return function () {
    return new AVar( value )
  }
}

exports._killVar = function ( util, error, avar ) {
  return function () {
    if ( avar.error == null ) {
      avar.error = error
      avar.value = AVar.EMPTY
      AVar.drainVar( util, avar )
    }
  }
}

exports._putVar = function ( util, value, avar, cb ) {
  return function () {
    var cell = AVar.putLast( avar.puts, { value: value, cb: cb } )
    AVar.drainVar( util, avar )
    return function () { // :: Effect Unit
      AVar.deleteCell( cell ) // a canceller that removes the callback function from the queue
    }
  }
}

exports._takeVar = function ( util, avar, cb ) {
  return function () {
    var cell = AVar.putLast( avar.takes, cb )
    AVar.drainVar( util, avar )
    return function () {
      AVar.deleteCell( cell )
    }
  }
}

exports._readVar = function ( util, avar, cb ) {
  return function () {
    var cell = AVar.putLast( avar.reads, cb )
    AVar.drainVar( util, avar )
    return function () {
      AVar.deleteCell( cell )
    }
  }
}

exports._tryPutVar = function ( util, value, avar ) {
  return function () {
    if ( avar.value == AVar.EMPTY && avar.error == null ) {
      avar.value = value
      AVar.drainVar( util, avar )
      return true
    } else {
      return false
    }
  }
}

exports._tryTakeVar = function ( util, avar ) {
  return function () {
    var value = avar.value
    if ( value == AVar.EMPTY ) {
      return util.nothing
    } else {
      avar.value = AVar.EMPTY
      AVar.drainVar( util, avar )
      return util.just( value )
    }
  }
}

exports._tryReadVar = function ( util, avar ) {
  return function () {
    if ( avar.value == AVar.EMPTY ) {
      return util.nothing
    } else {
      return util.just( avar.value )
    }
  }
}

exports._status = function ( util, avar ) {
  return function () {
    if ( avar.error ) {
      return util.killed( avar.error )
    }
    if ( avar.value == AVar.Empty ) {
      return util.empty
    }
    return util.filled( avar.value )
  }
}
