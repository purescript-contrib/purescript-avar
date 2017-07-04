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

function putQueue (queue, value) {
  var cell = new MutableCell(queue, value);
  cell.prev = queue.tail;
  queue.last.next = cell;
  queue.last = cell;
  queue.size++;
  return cell;
}

function insertQueue (queue, value) {
  var cell = new MutableCell(queue, value);
  cell.next = queue.head;
  queue.head.prev = cell;
  queue.head = cell;
  queue.size++;
  return cell;
}

function takeQueue (queue) {
  if (queue.size === 0) {
    return null;
  }
  var cell = queue.head;
  queue.head = cell.next;
  queue.head.prev = null;
  queue.size--;
  cell.next = null;
  cell.queue = null;
  return cell.value;
}

function deleteCell (cell) {
  if (cell.queue) {
    if (cell.prev) {
      cell.prev.next = cell.next;
    }
    if (cell.next) {
      cell.next.prev = cell.prev;
    }
    cell.queue = null;
    cell.value = null;
    cell.next  = null;
    cell.prev  = null;
  }
}

function AVar () {
  this.draining  = false;
  this.error     = null;
  this.value     = EMPTY;
  this.consumers = new MutableQueue();
  this.producers = new MutableQueue();
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

exports._killVar = function (left, right, error, avar) {
  return function () {
    if (avar.error === null) {
      avar.error = error;
      drainVar(left, right, avar);
    }
  };
};

exports._putVar = function (left, right, value, avar, cb) {
  return function () {
    if (avar.error !== null) {
      runEff(cb(left(avar.error)));
      return NO_EFFECT;
    }
    var cell = putQueue(avar.producers, { cb: cb, value: value });
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
    var cell = putQueue(avar.consumers, { cb: cb, peek: false, value: value });
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
    var cell = insertQueue(avar.consumers, { cb: cb, peek: true, value: value });
    drainVar(left, right, avar);
    return function () {
      deleteCell(cell);
    };
  }
};

exports._tryPutVar = function (left, right, value, avar) {
  return function () {
    if (avar.value === EMPTY && value.error === null) {
      putQueue(avar.queue, value);
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
    if (value === EMPTY || value.error !== null) {
      return nothing;
    } else {
      avar.value = EMPTY;
      drainVar(left, right, avar);
      return just(value);
    }
  };
};

exports._tryReadVar = function (nothing, just, avar) {
  if (avar.value === EMPTY || value.error !== null) {
    return nothing;
  } else {
    return just(avar.value);
  }
};

exports.isEmptyVar = function (avar) {
  return function () {
    return avar.value === EMPTY || avar.error !== null;
  };
};

function drainVar (left, right, avar) {
  if (avar.draining) {
    return;
  }

  var ps    = avar.producers;
  var cs    = avar.consumers;
  var value = avar.value;
  var p, c;

  avar.draining = true;

  if (avar.error === null) {
    while (1) {
      p = null;
      c = null;

      if (cs.size === 0 || ps.size === 0) {
        break;
      }

      if (value === EMPTY && (p = takeQueue(ps))) {
        value = avar.value = p.value;
      }

      if (value !== EMPTY) {
        value = right(value);
        while (c = takeQueue(cs)) {
          runEff(c.cb(value));
          if (!c.peek) {
            break;
          }
        }
        value = EMPTY;
      }

      if (p !== null) {
        runEff(p.cb(right(void 0)));
      }
    }
  }

  if (avar.error !== null) {
    value = left(avar.error);
    while (1) {
      if (ps.size === 0 && cs.size === 0) {
        break;
      }
      if (p = takeQueue(ps)) {
        runEff(p.cb(value));
      }
      while (c = takeQueue(cs)) {
        runEff(c.cb(value));
        if (!c.peek) {
          break;
        }
      }
    }
    break;
  }

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
