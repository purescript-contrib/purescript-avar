/* globals exports, setTimeout */
/* jshint -W097 */

"use strict";

var EMPTY = {};
var NO_EFFECT = function () {};

function MutableQueue () {
  this.head = null;
  this.last = null;
  this.size = 0;
}

function MutableCell (queue, value) {
  this.queue = queue;
  this.value = value;
  this.next  = null;
  this.prev  = null;
}

function putLast (queue, value) {
  var cell = new MutableCell(queue, value);
  switch (queue.size) {
  case 0:
    queue.head = cell;
    break;
  case 1:
    cell.prev = queue.head;
    queue.head.next = cell;
    queue.last = cell;
    break;
  default:
    cell.prev = queue.last;
    queue.last.next = cell;
    queue.last = cell;
  }
  queue.size++;
  return cell;
}

function takeLast (queue) {
  var cell;
  switch (queue.size) {
  case 0:
    return null;
  case 1:
    cell = queue.head;
    queue.head = null;
    break;
  case 2:
    cell = queue.last;
    queue.head.next = null;
    queue.last = null;
    break;
  default:
    cell = queue.last;
    queue.last = cell.prev;
    queue.last.next = null;
  }
  cell.prev = null;
  cell.queue = null;
  queue.size--;
  return cell.value;
}

function takeHead (queue) {
  var cell;
  switch (queue.size) {
  case 0:
    return null;
  case 1:
    cell = queue.head;
    queue.head = null;
    break;
  case 2:
    cell = queue.head;
    queue.last.prev = null;
    queue.head = queue.last;
    queue.last = null;
    break;
  default:
    cell = queue.head;
    queue.head = cell.next;
    queue.head.prev = null;
  }
  cell.next = null;
  cell.queue = null;
  queue.size--;
  return cell.value;
}

function deleteCell (cell) {
  if (cell.queue === null) {
    return;
  }
  if (cell.queue.last === cell) {
    takeLast(cell.queue);
    return;
  }
  if (cell.queue.head === cell) {
    takeHead(cell.queue);
    return;
  }
  if (cell.prev) {
    cell.prev.next = cell.next;
  }
  if (cell.next) {
    cell.next.prev = cell.prev;
  }
  cell.queue.size--;
  cell.queue = null;
  cell.value = null;
  cell.next  = null;
  cell.prev  = null;
}

function AVar () {
  this.draining = false;
  this.error    = null;
  this.value    = EMPTY;
  this.takes    = new MutableQueue();
  this.reads    = new MutableQueue();
  this.puts     = new MutableQueue();
}

exports.makeEmptyVar = function () {
  return new AVar();
};

exports.makeVar = function (value) {
  return function () {
    var avar = new AVar();
    avar.value = value;
    return avar;
  };
};

exports._killVar = function (left, right, avar, error) {
  return function () {
    if (avar.error === null) {
      avar.error = error;
      avar.value = EMPTY;
      drainVar(left, right, avar);
    }
  };
};

exports._putVar = function (left, right, avar, value, cb) {
  return function () {
    if (avar.error !== null) {
      runEff(cb(left(avar.error)));
      return NO_EFFECT;
    }
    var cell = putLast(avar.puts, { cb: cb, value: value });
    drainVar(left, right, avar);
    return function () {
      deleteCell(cell);
    };
  };
};

exports._takeVar = function (left, right, avar, cb) {
  return function () {
    if (avar.error !== null) {
      runEff(cb(left(avar.error)));
      return NO_EFFECT;
    }
    var cell = putLast(avar.takes, cb);
    drainVar(left, right, avar);
    return function () {
      deleteCell(cell);
    };
  };
};

exports._readVar = function (left, right, avar, cb) {
  return function () {
    if (avar.error !== null) {
      runEff(cb(left(avar.error)));
      return NO_EFFECT;
    }
    var cell = putLast(avar.reads, cb);
    drainVar(left, right, avar);
    return function () {
      deleteCell(cell);
    };
  };
};

exports._tryPutVar = function (left, right, avar, value) {
  return function () {
    if (avar.value === EMPTY && avar.error === null) {
      avar.value = value;
      drainVar(left, right, avar);
      return true;
    } else {
      return false;
    }
  };
};

exports._tryTakeVar = function (left, right, nothing, just, avar) {
  return function () {
    var value = avar.value;
    if (value === EMPTY) {
      return nothing;
    } else {
      avar.value = EMPTY;
      drainVar(left, right, avar);
      return just(value);
    }
  };
};

exports._tryReadVar = function (nothing, just, avar) {
  return function () {
    if (avar.value === EMPTY) {
      return nothing;
    } else {
      return just(avar.value);
    }
  };
};

exports.isEmptyVar = function (avar) {
  return function () {
    return avar.value === EMPTY;
  };
};

function drainVar (left, right, avar) {
  if (avar.draining) {
    return;
  }

  var ps = avar.puts;
  var ts = avar.takes;
  var rs = avar.reads;
  var p, r, t, value, rsize;

  avar.draining = true;

  /* jshint -W084 */
  while (1) {
    p = null;
    r = null;
    t = null;
    value = avar.value;
    rsize = rs.size;

    if (avar.error !== null) {
      value = left(avar.error);
      while (p = takeHead(ps)) {
        runEff(p.cb(value));
      }
      while (r = takeHead(rs)) {
        runEff(r(value));
      }
      while (t = takeHead(ts)) {
        runEff(t(value));
      }
      break;
    }

    // Process the next put. We do not immediately invoke the callback
    // because we want to preserve ordering. If there are takes/reads
    // we want to run those first.
    if (value === EMPTY && (p = takeHead(ps))) {
      avar.value = value = p.value;
    }

    if (value !== EMPTY) {
      // We go ahead and queue up the next take for the same reasons as
      // above. Invoking the read callbacks can affect the mutable queue.
      t = takeHead(ts);
      // We only want to process the reads queued up before running these
      // callbacks so we guard on rsize.
      while (rsize-- && (r = takeHead(rs))) {
        runEff(r(right(value)));
      }
      if (t !== null) {
        avar.value = EMPTY;
        runEff(t(right(value)));
      }
    }

    if (p !== null) {
      runEff(p.cb(right(void 0)));
    }

    // Callbacks could have queued up more items so we need to guard on the
    // actual mutable properties.
    if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) {
      break;
    }
  }
  /* jshint +W084 */

  avar.draining = false;
}

function runEff(eff) {
  try {
    eff();
  } catch (error) {
    setTimeout(function () {
      throw error;
    }, 0);
  }
}
