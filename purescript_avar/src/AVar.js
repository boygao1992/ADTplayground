/* globals exports, setTimeout */
"use strict"

var AVar = function () {

  // two-pointer queue
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

  function drainVar( util, avar ) {}

  AVar.EMPTY = EMPTY
  AVar.putLast = putLast
  AVar.takeLast = takeLast
  AVar.takeHead = takeHead
  AVar.deleteCell = deleteCell
  AVar.drainVar = drainVar
  return AVar
}

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
    var cell = AVar.putLast( avar.puts, { value, cb } )
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
