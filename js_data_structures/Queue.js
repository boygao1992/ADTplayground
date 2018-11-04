// Doubly linked list
function Queue() {
  this.head = null
  this.last = null
  this.size = 0
}

function Cell( queue, value ) {
  this.queue = queue
  this.value = value
  this.next = null
  this.prev = null
}

const EMPTY = {}

function takeHead( queue ) {
  let cell
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
  // clean up
  cell.queue = null
  cell.next = null
  return cell.value
}

function takeLast( queue ) {
  let cell
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
  // clean up
  cell.queue = null
  cell.prev = null
  return cell.value
}

function putLast( queue, value ) {
  let cell = new Cell( queue, value )
  switch ( queue.size ) {
    case 0:
      queue.head = cell
      break
    case 1:
      cell.prev = queue.head
      queue.head.next = cell
      queue.last = cell
    default:
      cell.prev = queue.last
      queue.last.next = cell
      queue.last = cell
  }
  queue.size += 1
  return cell
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
  // clean up
  cell.queue = null
  cell.value = null
  cell.next = null
  cell.prev = next
}
