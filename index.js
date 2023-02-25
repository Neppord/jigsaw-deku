(() => {
  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };
  var applyFlipped = function(x) {
    return function(f) {
      return f(x);
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map14 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map14(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map14 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map14($$const(x))(f);
      };
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label4) {
    return function(rec) {
      return rec[label4];
    };
  };
  var unsafeSet = function(label4) {
    return function(value12) {
      return function(rec) {
        var copy = {};
        for (var key2 in rec) {
          if ({}.hasOwnProperty.call(rec, key2)) {
            copy[key2] = rec[key2];
          }
        }
        copy[label4] = value12;
        return copy;
      };
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupUnit = {
    append: function(v) {
      return function(v1) {
        return unit;
      };
    }
  };
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map14 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map14($$const(identity2))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map14 = map(dictApply.Functor0());
    return function(f) {
      return function(a) {
        return function(b) {
          return apply1(map14(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply5 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply5(pure16(f))(a);
      };
    };
  };

  // output/Data.Array/foreign.js
  var range = function(start2) {
    return function(end) {
      var step3 = start2 > end ? -1 : 1;
      var result = new Array(step3 * (end - start2) + 1);
      var i = start2, n = 0;
      while (i !== end) {
        result[n++] = i;
        i += step3;
      }
      result[n] = i;
      return result;
    };
  };
  var replicateFill = function(count2) {
    return function(value12) {
      if (count2 < 1) {
        return [];
      }
      var result = new Array(count2);
      return result.fill(value12);
    };
  };
  var replicatePolyfill = function(count2) {
    return function(value12) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count2; i++) {
        result[n++] = value12;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count2 = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count2++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr4) {
      return function(xs) {
        return listToArray(foldr4(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i]))
              return just(i);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(l) {
          if (i < 0 || i >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i, 1);
          return just(l1);
        };
      };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from3, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to2 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from3, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to2);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();

  // output/Control.Bind/index.js
  var bind = function(dict) {
    return dict.bind;
  };

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind5(mb)(function(b) {
          return unless2(b)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    var pure9 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind5(f)(function(f$prime) {
          return bind5(a)(function(a$prime) {
            return pure9(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq4) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq4 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Semiring/index.js
  var zeroRecord = function(dict) {
    return dict.zeroRecord;
  };
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringRecordNil = {
    addRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    },
    mulRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    },
    oneRecord: function(v) {
      return function(v1) {
        return {};
      };
    },
    zeroRecord: function(v) {
      return function(v1) {
        return {};
      };
    }
  };
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var oneRecord = function(dict) {
    return dict.oneRecord;
  };
  var one = function(dict) {
    return dict.one;
  };
  var mulRecord = function(dict) {
    return dict.mulRecord;
  };
  var mul = function(dict) {
    return dict.mul;
  };
  var addRecord = function(dict) {
    return dict.addRecord;
  };
  var semiringRecord = function() {
    return function(dictSemiringRecord) {
      return {
        add: addRecord(dictSemiringRecord)($$Proxy.value),
        mul: mulRecord(dictSemiringRecord)($$Proxy.value),
        one: oneRecord(dictSemiringRecord)($$Proxy.value)($$Proxy.value),
        zero: zeroRecord(dictSemiringRecord)($$Proxy.value)($$Proxy.value)
      };
    };
  };
  var add = function(dict) {
    return dict.add;
  };
  var semiringRecordCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(dictSemiringRecord) {
        var addRecord1 = addRecord(dictSemiringRecord);
        var mulRecord1 = mulRecord(dictSemiringRecord);
        var oneRecord1 = oneRecord(dictSemiringRecord);
        var zeroRecord1 = zeroRecord(dictSemiringRecord);
        return function(dictSemiring) {
          var add1 = add(dictSemiring);
          var mul1 = mul(dictSemiring);
          var one1 = one(dictSemiring);
          var zero1 = zero(dictSemiring);
          return {
            addRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = addRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var insert10 = unsafeSet(key2);
                  var get2 = unsafeGet(key2);
                  return insert10(add1(get2(ra))(get2(rb)))(tail2);
                };
              };
            },
            mulRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = mulRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var insert10 = unsafeSet(key2);
                  var get2 = unsafeGet(key2);
                  return insert10(mul1(get2(ra))(get2(rb)))(tail2);
                };
              };
            },
            oneRecord: function(v) {
              return function(v1) {
                var tail2 = oneRecord1($$Proxy.value)($$Proxy.value);
                var key2 = reflectSymbol2($$Proxy.value);
                var insert10 = unsafeSet(key2);
                return insert10(one1)(tail2);
              };
            },
            zeroRecord: function(v) {
              return function(v1) {
                var tail2 = zeroRecord1($$Proxy.value)($$Proxy.value);
                var key2 = reflectSymbol2($$Proxy.value);
                var insert10 = unsafeSet(key2);
                return insert10(zero1)(tail2);
              };
            }
          };
        };
      };
    };
  };

  // output/Data.Ring/index.js
  var semiringRecord2 = /* @__PURE__ */ semiringRecord();
  var subRecord = function(dict) {
    return dict.subRecord;
  };
  var sub = function(dict) {
    return dict.sub;
  };
  var ringRecordNil = {
    subRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    },
    SemiringRecord0: function() {
      return semiringRecordNil;
    }
  };
  var ringRecordCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    var semiringRecordCons2 = semiringRecordCons(dictIsSymbol)();
    return function() {
      return function(dictRingRecord) {
        var subRecord1 = subRecord(dictRingRecord);
        var semiringRecordCons1 = semiringRecordCons2(dictRingRecord.SemiringRecord0());
        return function(dictRing) {
          var sub12 = sub(dictRing);
          var semiringRecordCons22 = semiringRecordCons1(dictRing.Semiring0());
          return {
            subRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = subRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var insert10 = unsafeSet(key2);
                  var get2 = unsafeGet(key2);
                  return insert10(sub12(get2(ra))(get2(rb)))(tail2);
                };
              };
            },
            SemiringRecord0: function() {
              return semiringRecordCons22;
            }
          };
        };
      };
    };
  };
  var ringRecord = function() {
    return function(dictRingRecord) {
      var semiringRecord1 = semiringRecord2(dictRingRecord.SemiringRecord0());
      return {
        sub: subRecord(dictRingRecord)($$Proxy.value),
        Semiring0: function() {
          return semiringRecord1;
        }
      };
    };
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidUnit = {
    mempty: unit,
    Semigroup0: function() {
      return semigroupUnit;
    }
  };
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);
  var lift22 = /* @__PURE__ */ lift2(applyEffect);
  var semigroupEffect = function(dictSemigroup) {
    return {
      append: lift22(append(dictSemigroup))
    };
  };
  var monoidEffect = function(dictMonoid) {
    var semigroupEffect1 = semigroupEffect(dictMonoid.Semigroup0());
    return {
      mempty: pureE(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupEffect1;
      }
    };
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref) {
      return function() {
        ref.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read2 = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl2 = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write2 = function(a) {
    return function(ref) {
      return function() {
        return ref.value = a;
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy2 = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
  var modify$prime2 = modifyImpl2;
  var modify2 = function(f) {
    return modify$prime2(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });
  var applyST = /* @__PURE__ */ $lazy_applyST(47);

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  var pushAll = function(as) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as);
      };
    };
  };
  var splice = function(i) {
    return function(howMany) {
      return function(bs) {
        return function(xs) {
          return function() {
            return xs.splice.apply(xs, [i, howMany].concat(bs));
          };
        };
      };
    };
  };
  function copyImpl(xs) {
    return function() {
      return xs.slice();
    };
  }
  var freeze = copyImpl;
  var sortByImpl2 = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from3, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to2 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from3, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to2);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare3, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Array.ST/index.js
  var push = function(a) {
    return pushAll([a]);
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var semiringTuple = function(dictSemiring) {
    var add5 = add(dictSemiring);
    var one2 = one(dictSemiring);
    var mul3 = mul(dictSemiring);
    var zero2 = zero(dictSemiring);
    return function(dictSemiring1) {
      var add1 = add(dictSemiring1);
      var mul1 = mul(dictSemiring1);
      return {
        add: function(v) {
          return function(v1) {
            return new Tuple(add5(v.value0)(v1.value0), add1(v.value1)(v1.value1));
          };
        },
        one: new Tuple(one2, one(dictSemiring1)),
        mul: function(v) {
          return function(v1) {
            return new Tuple(mul3(v.value0)(v1.value0), mul1(v.value1)(v1.value1));
          };
        },
        zero: new Tuple(zero2, zero(dictSemiring1))
      };
    };
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq4 = eq(dictEq);
    return function(dictEq1) {
      var eq13 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq4(x.value0)(y.value0) && eq13(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare3 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare3(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };
  var curry = function(f) {
    return function(a) {
      return function(b) {
        return f(new Tuple(a, b));
      };
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var wrap = function() {
    return coerce2;
  };
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var oneOf = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictPlus) {
      return foldr22(alt(dictPlus.Alt0()))(empty(dictPlus));
    };
  };
  var oneOfMap = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictPlus) {
      var alt8 = alt(dictPlus.Alt0());
      var empty8 = empty(dictPlus);
      return function(f) {
        return foldr22(function($453) {
          return alt8(f($453));
        })(empty8);
      };
    };
  };
  var traverse_ = function(dictApplicative) {
    var applySecond3 = applySecond(dictApplicative.Apply0());
    var pure9 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond3(f($454));
        })(pure9(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append6 = append(dictMonoid.Semigroup0());
      var mempty4 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(v1) {
              if (v.init) {
                return {
                  init: false,
                  acc: v1
                };
              }
              ;
              return {
                init: false,
                acc: append6(v.acc)(append6(sep)(v1))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty4
          })(xs).acc;
        };
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty4 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty4;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append6 = append(dictMonoid.Semigroup0());
      var mempty4 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append6(f(x))(acc);
          };
        })(mempty4);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply5) {
      return function(map14) {
        return function(pure9) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure9([]);
                  case 1:
                    return map14(array1)(f(array[bot]));
                  case 2:
                    return apply5(map14(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply5(apply5(map14(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply5(map14(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse2 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse2(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var $$for = function(dictApplicative) {
    return function(dictTraversable) {
      var traverse2 = traverse(dictTraversable)(dictApplicative);
      return function(x) {
        return function(f) {
          return traverse2(f)(x);
        };
      };
    };
  };

  // output/Data.Array/index.js
  var intercalate1 = /* @__PURE__ */ intercalate(foldableArray);
  var fromJust2 = /* @__PURE__ */ fromJust();
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var intercalate2 = function(dictMonoid) {
    return intercalate1(dictMonoid);
  };
  var index = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i) {
          return fromJust2(deleteAt(i)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };

  // output/Data.FoldableWithIndex/index.js
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldlWithIndex = function(dict) {
    return dict.foldlWithIndex;
  };
  var foldMapWithIndex = function(dict) {
    return dict.foldMapWithIndex;
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty4 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty4);
      };
    }
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var singleton3 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var lookup = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare3(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare3(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare3(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up2 = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down3 = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up2(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare3(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        return down3(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      var up2 = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up2(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up2(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down3 = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare3(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up2(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = compare3(k)(m.value4);
              var v3 = compare3(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down3(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty4 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty4;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(f(m.value4)(m.value5)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value4)(foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty4 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty4;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append22(f(m.value1)(m.value2))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append22(f(m.value1)(m.value2))(append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3))(append22(f(m.value4)(m.value5))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): " + [m.constructor.name]);
        };
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var foldrWithIndex2 = /* @__PURE__ */ foldrWithIndex(foldableWithIndexMap);
  var foldlWithIndex2 = /* @__PURE__ */ foldlWithIndex(foldableWithIndexMap);
  var keys = /* @__PURE__ */ function() {
    return foldrWithIndex2(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  }();
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var pop1 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop1(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup1 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert1 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup1(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert1(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };
  var unionWith = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(m1) {
        return function(m2) {
          var go2 = function(k) {
            return function(m) {
              return function(v) {
                return alter1(function() {
                  var $936 = maybe(v)(f(v));
                  return function($937) {
                    return Just.create($936($937));
                  };
                }())(k)(m);
              };
            };
          };
          return foldlWithIndex2(go2)(m2)(m1);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };

  // output/Data.Compactable/index.js
  var compact = function(dict) {
    return dict.compact;
  };

  // output/Data.Profunctor/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };
  var dimap = function(dict) {
    return dict.dimap;
  };
  var lcmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(a2b) {
      return dimap1(a2b)(identity5);
    };
  };

  // output/Data.Set/index.js
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList);
  var foldl2 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var union2 = function(dictOrd) {
    var union1 = union(dictOrd);
    return function(v) {
      return function(v1) {
        return union1(v)(v1);
      };
    };
  };
  var toList = function(v) {
    return keys(v);
  };
  var singleton4 = function(a) {
    return singleton3(a)(unit);
  };
  var semigroupSet = function(dictOrd) {
    return {
      append: union2(dictOrd)
    };
  };
  var insert2 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(a) {
      return function(v) {
        return insert1(a)(unit)(v);
      };
    };
  };
  var foldableSet = {
    foldMap: function(dictMonoid) {
      var foldMap12 = foldMap2(dictMonoid);
      return function(f) {
        var $129 = foldMap12(f);
        return function($130) {
          return $129(toList($130));
        };
      };
    },
    foldl: function(f) {
      return function(x) {
        var $131 = foldl2(f)(x);
        return function($132) {
          return $131(toList($132));
        };
      };
    },
    foldr: function(f) {
      return function(x) {
        var $133 = foldr2(f)(x);
        return function($134) {
          return $133(toList($134));
        };
      };
    }
  };
  var empty3 = empty2;
  var monoidSet = function(dictOrd) {
    var semigroupSet1 = semigroupSet(dictOrd);
    return {
      mempty: empty3,
      Semigroup0: function() {
        return semigroupSet1;
      }
    };
  };
  var $$delete2 = function(dictOrd) {
    var delete1 = $$delete(dictOrd);
    return function(a) {
      return function(v) {
        return delete1(a)(v);
      };
    };
  };

  // output/FRP.Event/foreign.js
  var fastForeachE = (as, f) => {
    for (var i = 0, l = as.length; i < l; i++) {
      f(as[i]);
    }
  };
  var fastForeachOhE = (o, f) => {
    for (const a in o) {
      f(o[a]);
    }
  };
  var objHack = () => ({});
  var insertObjHack = (k, v, o) => {
    o[k] = v;
  };
  var deleteObjHack = (k, o) => {
    delete o[k];
  };

  // output/Control.Monad.ST.Global/index.js
  var toEffect = unsafeCoerce2;

  // output/Control.Monad.ST.Class/index.js
  var monadSTEffect = {
    liftST: toEffect,
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftST = function(dict) {
    return dict.liftST;
  };

  // output/Data.Filterable/index.js
  var filterMap = function(dict) {
    return dict.filterMap;
  };
  var filter4 = function(dict) {
    return dict.filter;
  };

  // output/Effect.Timer/foreign.js
  function setTimeoutImpl(ms) {
    return function(fn) {
      return function() {
        return setTimeout(fn, ms);
      };
    };
  }
  function clearTimeoutImpl(id) {
    return function() {
      clearTimeout(id);
    };
  }

  // output/Effect.Timer/index.js
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var setTimeout2 = setTimeoutImpl;
  var eqTimeoutId = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordTimeoutId = {
    compare: function(x) {
      return function(y) {
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqTimeoutId;
    }
  };
  var clearTimeout2 = clearTimeoutImpl;

  // output/Effect.Uncurried/foreign.js
  var mkEffectFn1 = function mkEffectFn12(fn) {
    return function(x) {
      return fn(x)();
    };
  };
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };

  // output/Effect.Uncurried/index.js
  var semigroupEffectFn1 = function(dictSemigroup) {
    var append6 = append(semigroupEffect(dictSemigroup));
    return {
      append: function(f1) {
        return function(f2) {
          return mkEffectFn1(function(a) {
            return append6(runEffectFn1(f1)(a))(runEffectFn1(f2)(a));
          });
        };
      }
    };
  };
  var monoidEffectFn1 = function(dictMonoid) {
    var mempty4 = mempty(monoidEffect(dictMonoid));
    var semigroupEffectFn11 = semigroupEffectFn1(dictMonoid.Semigroup0());
    return {
      mempty: mkEffectFn1(function(v) {
        return mempty4;
      }),
      Semigroup0: function() {
        return semigroupEffectFn11;
      }
    };
  };

  // output/FRP.Event.Class/index.js
  var map3 = /* @__PURE__ */ map(functorTuple);
  var pure2 = /* @__PURE__ */ pure(applicativeMaybe);
  var sampleOnRight = function(dict) {
    return dict.sampleOnRight;
  };
  var sampleOnRightOp = function(dictIsEvent) {
    var sampleOnRight1 = sampleOnRight(dictIsEvent);
    var map14 = map(dictIsEvent.Filterable1().Functor1());
    return function(ef) {
      return function(ea) {
        return sampleOnRight1(ef)(map14(applyFlipped)(ea));
      };
    };
  };
  var keepLatest = function(dict) {
    return dict.keepLatest;
  };
  var gateBy = function(dictIsEvent) {
    var Filterable1 = dictIsEvent.Filterable1();
    var compact3 = compact(Filterable1.Compactable0());
    var sampleOnRightOp1 = sampleOnRightOp(dictIsEvent);
    var map14 = map(Filterable1.Functor1());
    var Alternative0 = dictIsEvent.Alternative0();
    var alt8 = alt(Alternative0.Plus1().Alt0());
    var pure16 = pure(Alternative0.Applicative0());
    return function(f) {
      return function(sampled) {
        return function(sampler) {
          return compact3(sampleOnRightOp1(map14(function(p) {
            return function(x) {
              var $80 = f(p)(x);
              if ($80) {
                return new Just(x);
              }
              ;
              return Nothing.value;
            };
          })(alt8(pure16(Nothing.value))(map14(Just.create)(sampled))))(sampler));
        };
      };
    };
  };
  var gate = function(dictIsEvent) {
    return gateBy(dictIsEvent)(function(x) {
      return function(v) {
        return fromMaybe(false)(x);
      };
    });
  };
  var fix2 = function(dict) {
    return dict.fix;
  };
  var fold2 = function(dictIsEvent) {
    var fix1 = fix2(dictIsEvent);
    var sampleOnRight1 = sampleOnRight(dictIsEvent);
    var Alternative0 = dictIsEvent.Alternative0();
    var alt8 = alt(Alternative0.Plus1().Alt0());
    var pure16 = pure(Alternative0.Applicative0());
    var map14 = map(dictIsEvent.Filterable1().Functor1());
    return function(f) {
      return function(b) {
        return function(e) {
          return fix1(function(i) {
            return sampleOnRight1(alt8(i)(pure16(b)))(map14(flip(f))(e));
          });
        };
      };
    };
  };
  var mapAccum = function(dictIsEvent) {
    var filterMap2 = filterMap(dictIsEvent.Filterable1());
    var fold12 = fold2(dictIsEvent);
    return function(f) {
      return function(acc) {
        return function(xs) {
          return filterMap2(snd)(fold12(function(v) {
            return function(b) {
              return map3(pure2)(f(v.value0)(b));
            };
          })(new Tuple(acc, Nothing.value))(xs));
        };
      };
    };
  };

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a) {
    return function(b) {
      return a === b;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/FRP.Event/index.js
  var $runtime_lazy3 = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
  var for_2 = /* @__PURE__ */ for_(applicativeEffect);
  var for_1 = /* @__PURE__ */ for_2(foldableMaybe);
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidEffectFn1(monoidUnit));
  var liftST2 = /* @__PURE__ */ liftST(monadSTEffect);
  var mempty1 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidSet(ordTimeoutId));
  var $$delete3 = /* @__PURE__ */ $$delete2(ordTimeoutId);
  var append1 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupSet(ordTimeoutId));
  var for_22 = /* @__PURE__ */ for_2(foldableSet);
  var apply2 = /* @__PURE__ */ apply(applyEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var sampleOnRight2 = function(v) {
    return function(v1) {
      return function(b, k) {
        var latest = $$new(Nothing.value)();
        var c1 = v(b, function(a) {
          return write(new Just(a))(latest)();
        });
        var c2 = v1(b, function(f) {
          var o = read(latest)();
          return for_1(o)(function(a) {
            return function() {
              return k(f(a));
            };
          })();
        });
        return function __do4() {
          c1();
          return c2();
        };
      };
    };
  };
  var sampleOnLeft = function(v) {
    return function(v1) {
      return function(b, k) {
        var latest = $$new(Nothing.value)();
        var c1 = v(b, function(a) {
          var o = read(latest)();
          return for_1(o)(function(f) {
            return function() {
              return k(f(a));
            };
          })();
        });
        var c2 = v1(b, function(f) {
          return write(new Just(f))(latest)();
        });
        return function __do4() {
          c1();
          return c2();
        };
      };
    };
  };
  var mailbox$prime = function(dictOrd) {
    var alter2 = alter(dictOrd);
    var lookup3 = lookup(dictOrd);
    return function __do4() {
      var r = $$new(empty2)();
      return {
        event: function(a) {
          return function(v, k2) {
            $$void3(modify(alter2(function(v1) {
              if (v1 instanceof Nothing) {
                return new Just([k2]);
              }
              ;
              if (v1 instanceof Just) {
                return new Just(append2(v1.value0)([k2]));
              }
              ;
              throw new Error("Failed pattern match at FRP.Event (line 554, column 17 - line 556, column 51): " + [v1.constructor.name]);
            })(a))(r))();
            return $$void3(modify(alter2(function(v1) {
              if (v1 instanceof Nothing) {
                return Nothing.value;
              }
              ;
              if (v1 instanceof Just) {
                return new Just(deleteBy(unsafeRefEq)(k2)(v1.value0));
              }
              ;
              throw new Error("Failed pattern match at FRP.Event (line 563, column 17 - line 565, column 65): " + [v1.constructor.name]);
            })(a))(r));
          };
        },
        push: function(v) {
          var o = read(r)();
          var v1 = lookup3(v.address)(o);
          if (v1 instanceof Nothing) {
            return unit;
          }
          ;
          if (v1 instanceof Just) {
            return fastForeachE(v1.value0, function(i) {
              return i(v.payload);
            });
          }
          ;
          throw new Error("Failed pattern match at FRP.Event (line 572, column 9 - line 574, column 95): " + [v1.constructor.name]);
        }
      };
    };
  };
  var keepLatest2 = function(v) {
    return function(tf, k) {
      var cancelInner = $$new(pure3(unit))();
      var cancelOuter = v(tf, function(v1) {
        var ci = read(cancelInner)();
        ci();
        var c = v1(tf, k);
        return write(c)(cancelInner)();
      });
      return function __do4() {
        var ci = read(cancelInner)();
        ci();
        return cancelOuter();
      };
    };
  };
  var functorEvent = {
    map: function(f) {
      return function(v) {
        return function(b, k) {
          return v(b, function(a) {
            return k(f(a));
          });
        };
      };
    }
  };
  var map1 = /* @__PURE__ */ map(functorEvent);
  var filter5 = function(p) {
    return function(v) {
      return function(tf, k) {
        return v(tf, function(a) {
          var v1 = p(a);
          if (v1 instanceof Just) {
            return k(v1.value0);
          }
          ;
          if (v1 instanceof Nothing) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at FRP.Event (line 211, column 31 - line 213, column 35): " + [v1.constructor.name]);
        });
      };
    };
  };
  var filter$prime = function(f) {
    return filter5(function(a) {
      var v = f(a);
      if (v) {
        return new Just(a);
      }
      ;
      if (!v) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at FRP.Event (line 139, column 13 - line 141, column 25): " + [v.constructor.name]);
    });
  };
  var create$prime = function __do() {
    var subscribers = objHack();
    var idx = $$new(0)();
    return {
      event: function(v, k) {
        var rk = $$new(k)();
        var ix = read(idx)();
        insertObjHack(ix, rk, subscribers);
        modify_(function(v1) {
          return v1 + 1 | 0;
        })(idx)();
        return function __do4() {
          write(mempty2)(rk)();
          deleteObjHack(ix, subscribers);
          return unit;
        };
      },
      push: function(a) {
        return fastForeachOhE(subscribers, function(rk) {
          var k = read(rk)();
          return k(a);
        });
      }
    };
  };
  var fix3 = function(f) {
    return function(tf, k) {
      var v = create$prime();
      var v1 = f(v.event);
      var c2 = v.event(tf, k);
      var c1 = v1(tf, v.push);
      return function __do4() {
        c1();
        return c2();
      };
    };
  };
  var compactableEvent = {
    compact: /* @__PURE__ */ filter5(/* @__PURE__ */ identity(categoryFn)),
    separate: function(xs) {
      return {
        left: filter5(function(v) {
          if (v instanceof Left) {
            return new Just(v.value0);
          }
          ;
          if (v instanceof Right) {
            return Nothing.value;
          }
          ;
          throw new Error("Failed pattern match at FRP.Event (line 122, column 13 - line 124, column 33): " + [v.constructor.name]);
        })(xs),
        right: filter5(function(v) {
          if (v instanceof Right) {
            return new Just(v.value0);
          }
          ;
          if (v instanceof Left) {
            return Nothing.value;
          }
          ;
          throw new Error("Failed pattern match at FRP.Event (line 129, column 13 - line 131, column 32): " + [v.constructor.name]);
        })(xs)
      };
    }
  };
  var filterableEvent = {
    filter: filter$prime,
    filterMap: filter5,
    partition: function(p) {
      return function(xs) {
        return {
          yes: filter$prime(p)(xs),
          no: filter$prime(function($226) {
            return !p($226);
          })(xs)
        };
      };
    },
    partitionMap: function(f) {
      return function(xs) {
        return {
          left: filterMap(filterableEvent)(function() {
            var $227 = either(Just.create)($$const(Nothing.value));
            return function($228) {
              return $227(f($228));
            };
          }())(xs),
          right: filterMap(filterableEvent)(function($229) {
            return hush(f($229));
          })(xs)
        };
      };
    },
    Compactable0: function() {
      return compactableEvent;
    },
    Functor1: function() {
      return functorEvent;
    }
  };
  var biSampleOn = function(v) {
    return function(v1) {
      return function(tf, k) {
        var latest1 = $$new(Nothing.value)();
        var replay1 = liftST2(newSTArray)();
        var latest2 = $$new(Nothing.value)();
        var replay2 = liftST2(newSTArray)();
        var capturing = $$new(true)();
        var c1 = v(tf, function(a) {
          var o = read(capturing)();
          if (o) {
            return $$void3(liftST2(push(a)(replay1)))();
          }
          ;
          write(new Just(a))(latest1)();
          var res = read(latest2)();
          return for_1(res)(function(f) {
            return function() {
              return k(f(a));
            };
          })();
        });
        var c2 = v1(tf, function(f) {
          var o = read(capturing)();
          if (o) {
            return $$void3(liftST2(push(f)(replay2)))();
          }
          ;
          write(new Just(f))(latest2)();
          var res = read(latest1)();
          return for_1(res)(function(a) {
            return function() {
              return k(f(a));
            };
          })();
        });
        write(false)(capturing)();
        var samples1 = liftST2(freeze(replay1))();
        var samples2 = liftST2(freeze(replay2))();
        (function() {
          if (samples1.length === 0) {
            return write(last(samples2))(latest2)();
          }
          ;
          return fastForeachE(samples1, function(a) {
            write(new Just(a))(latest1)();
            return fastForeachE(samples2, function(f) {
              write(new Just(f))(latest2)();
              return k(f(a));
            });
          });
        })();
        liftST2(splice(0)(length(samples1))([])(replay1))();
        liftST2(splice(0)(length(samples2))([])(replay2))();
        return function __do4() {
          c1();
          return c2();
        };
      };
    };
  };
  var subscribe = function(i) {
    return function(v) {
      return v;
    }($lazy_backdoor(331).subscribe)(i);
  };
  var $lazy_backdoor = /* @__PURE__ */ $runtime_lazy3("backdoor", "FRP.Event", function() {
    var create_ = function __do4() {
      var subscribers = objHack();
      var idx = $$new(0)();
      return {
        event: function(v, k) {
          var rk = $$new(k)();
          var ix = read(idx)();
          insertObjHack(ix, rk, subscribers);
          modify_(function(v1) {
            return v1 + 1 | 0;
          })(idx)();
          return function __do5() {
            write(mempty2)(rk)();
            deleteObjHack(ix, subscribers);
            return unit;
          };
        },
        push: function(a) {
          return function() {
            return fastForeachOhE(subscribers, function(rk) {
              var k = read(rk)();
              return k(a);
            });
          };
        }
      };
    };
    return {
      createO: create$prime,
      makeEvent: function() {
        var makeEvent_ = function(e) {
          return function(tf, k) {
            if (tf) {
              return pure3(unit);
            }
            ;
            return e(function(a) {
              return function() {
                return k(a);
              };
            })();
          };
        };
        return makeEvent_;
      }(),
      makeEventO: function() {
        var makeEventO_ = function(e) {
          return function(tf, k) {
            if (tf) {
              return pure3(unit);
            }
            ;
            return e(k);
          };
        };
        return makeEventO_;
      }(),
      makePureEvent: function() {
        var makePureEvent_ = function(e) {
          return function(v, k) {
            return e(function(a) {
              return function() {
                return k(a);
              };
            })();
          };
        };
        return makePureEvent_;
      }(),
      makeLemmingEvent: function() {
        var makeLemmingEvent_ = function(e) {
          return function(tf, k) {
            var o = function(v) {
              return function(kx) {
                return function() {
                  return v(tf, mkEffectFn1(kx));
                };
              };
            };
            return e(o)(function(a) {
              return function() {
                return k(a);
              };
            })();
          };
        };
        return makeLemmingEvent_;
      }(),
      makeLemmingEventO: function() {
        var makeLemmingEventO_ = function(e) {
          return function(tf, k) {
            var o = function(v, kx) {
              return v(tf, kx);
            };
            return e(o, k);
          };
        };
        return makeLemmingEventO_;
      }(),
      create: create_,
      createPure: create_,
      createPureO: create$prime,
      subscribe: function() {
        var subscribe_ = function(v) {
          return function(k) {
            return function() {
              return v(false, mkEffectFn1(k));
            };
          };
        };
        return subscribe_;
      }(),
      subscribeO: function() {
        var subscribeO_ = function(v, k) {
          return v(false, k);
        };
        return subscribeO_;
      }(),
      subscribePureO: function() {
        var subscribePureO_ = function(v, k) {
          return v(true, k);
        };
        return subscribePureO_;
      }(),
      subscribePure: function() {
        var subscribePure_ = function() {
          var o = function(v) {
            return function(k) {
              return function() {
                return v(true, mkEffectFn1(k));
              };
            };
          };
          return o;
        }();
        return subscribePure_;
      }(),
      bus: function() {
        var bus_ = function(f) {
          return function(v, k) {
            var v1 = $lazy_create(804)();
            k(f(v1.push)(v1.event));
            return pure3(unit);
          };
        };
        return bus_;
      }(),
      memoize: function() {
        var memoize_ = function(v) {
          return function(f) {
            return function(b, k) {
              var v1 = create$prime();
              k(f(v1.event));
              return v(b, v1.push);
            };
          };
        };
        return memoize_;
      }(),
      hot: function() {
        var hot_ = function(e) {
          return function __do4() {
            var v = $lazy_create(822)();
            var unsubscribe = subscribe(e)(v.push)();
            return {
              event: v.event,
              unsubscribe
            };
          };
        };
        return hot_;
      }(),
      mailbox: function() {
        var mailbox_ = function(dictOrd) {
          return function __do4() {
            var v = mailbox$prime(dictOrd)();
            return {
              event: v.event,
              push: function(k) {
                return function() {
                  return v.push(k);
                };
              }
            };
          };
        };
        return mailbox_;
      }(),
      mailboxed: function() {
        var mailboxed_ = function(dictOrd) {
          var mailbox$prime1 = mailbox$prime(dictOrd);
          return function(v) {
            return function(f) {
              return function(b, k) {
                var v1 = mailbox$prime1();
                k(f(v1.event));
                return v(b, v1.push);
              };
            };
          };
        };
        return mailboxed_;
      }(),
      delay: function() {
        var delay_ = function(n) {
          return function(v) {
            return function(tf, k) {
              var tid = $$new(mempty1)();
              var canceler = v(tf, function(a) {
                var localId = $$new(Nothing.value)();
                var id = setTimeout2(n)(function __do4() {
                  k(a);
                  var lid = read(localId)();
                  return maybe(pure3(unit))(function(id2) {
                    return modify_($$delete3(id2))(tid);
                  })(lid)();
                })();
                write(new Just(id))(localId)();
                return modify_(append1(singleton4(id)))(tid)();
              });
              return function __do4() {
                var ids = read(tid)();
                for_22(ids)(clearTimeout2)();
                return canceler();
              };
            };
          };
        };
        return delay_;
      }()
    };
  });
  var $lazy_create = /* @__PURE__ */ $runtime_lazy3("create", "FRP.Event", function() {
    return function __do4() {
      unit;
      return function(v) {
        return v;
      }($lazy_backdoor(447).create)();
    };
  });
  var backdoor = /* @__PURE__ */ $lazy_backdoor(663);
  var bus = function(i) {
    return function(v) {
      return v;
    }(backdoor.bus)(i);
  };
  var mailboxed = function(dictOrd) {
    return function(i) {
      return function(v) {
        return v(dictOrd);
      }(backdoor.mailboxed)(i);
    };
  };
  var makeEvent = function(i) {
    return function(v) {
      return v;
    }(backdoor.makeEvent)(i);
  };
  var makeLemmingEventO = function(i) {
    return function(v) {
      return v;
    }(backdoor.makeLemmingEventO)(i);
  };
  var memoize = function(i) {
    return function(v) {
      return v;
    }(backdoor.memoize)(i);
  };
  var applyEvent = {
    apply: function(a) {
      return function(b) {
        return biSampleOn(a)(map1(applyFlipped)(b));
      };
    },
    Functor0: function() {
      return functorEvent;
    }
  };
  var lift23 = /* @__PURE__ */ lift2(applyEvent);
  var applicativeEvent = {
    pure: function(a) {
      return function(v, k) {
        k(a);
        return pure3(unit);
      };
    },
    Apply0: function() {
      return applyEvent;
    }
  };
  var pure22 = /* @__PURE__ */ pure(applicativeEvent);
  var heytingAlgebraEvent = function(dictHeytingAlgebra) {
    return {
      tt: pure22(tt(dictHeytingAlgebra)),
      ff: pure22(ff(dictHeytingAlgebra)),
      not: map1(not(dictHeytingAlgebra)),
      implies: lift23(implies(dictHeytingAlgebra)),
      conj: lift23(conj(dictHeytingAlgebra)),
      disj: lift23(disj(dictHeytingAlgebra))
    };
  };
  var semiringEvent = function(dictSemiring) {
    return {
      zero: pure22(zero(dictSemiring)),
      one: pure22(one(dictSemiring)),
      add: lift23(add(dictSemiring)),
      mul: lift23(mul(dictSemiring))
    };
  };
  var ringEvent = function(dictRing) {
    var semiringEvent1 = semiringEvent(dictRing.Semiring0());
    return {
      sub: lift23(sub(dictRing)),
      Semiring0: function() {
        return semiringEvent1;
      }
    };
  };
  var altEvent = {
    alt: function(v) {
      return function(v1) {
        return function(tf, k) {
          return apply2(map4(function(v2) {
            return function(v3) {
              return function __do4() {
                v2();
                return v3();
              };
            };
          })(function() {
            return v(tf, k);
          }))(function() {
            return v1(tf, k);
          })();
        };
      };
    },
    Functor0: function() {
      return functorEvent;
    }
  };
  var plusEvent = {
    empty: function(v, v1) {
      return pure3(unit);
    },
    Alt0: function() {
      return altEvent;
    }
  };
  var alternativeEvent = {
    Applicative0: function() {
      return applicativeEvent;
    },
    Plus1: function() {
      return plusEvent;
    }
  };
  var eventIsEvent = {
    keepLatest: keepLatest2,
    sampleOnRight: sampleOnRight2,
    sampleOnLeft,
    fix: fix3,
    Alternative0: function() {
      return alternativeEvent;
    },
    Filterable1: function() {
      return filterableEvent;
    }
  };

  // output/Deku.Attribute/index.js
  var pure4 = /* @__PURE__ */ pure(applicativeEvent);
  var Prop$prime = /* @__PURE__ */ function() {
    function Prop$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Prop$prime2.create = function(value0) {
      return new Prop$prime2(value0);
    };
    return Prop$prime2;
  }();
  var Cb$prime = /* @__PURE__ */ function() {
    function Cb$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Cb$prime2.create = function(value0) {
      return new Cb$prime2(value0);
    };
    return Cb$prime2;
  }();
  var Unset$prime = /* @__PURE__ */ function() {
    function Unset$prime2() {
    }
    ;
    Unset$prime2.value = new Unset$prime2();
    return Unset$prime2;
  }();
  var Attribute = function(x) {
    return x;
  };
  var unsafeUnAttribute = /* @__PURE__ */ coerce();
  var unsafeAttribute = Attribute;
  var prop$prime = /* @__PURE__ */ function() {
    return Prop$prime.create;
  }();
  var cb$prime = /* @__PURE__ */ function() {
    return Cb$prime.create;
  }();
  var attr = function(dict) {
    return dict.attr;
  };
  var mapAttr = function(dictFunctor) {
    var map14 = map(dictFunctor);
    return function(dictAttr) {
      var attr1 = attr(dictAttr);
      return function(a) {
        return function(b) {
          return map14(function(v) {
            return attr1(a)(v);
          })(b);
        };
      };
    };
  };
  var pureAttr = function(dictAttr) {
    var attr1 = attr(dictAttr);
    return function(a) {
      return function(b) {
        return pure4(attr1(a)(b));
      };
    };
  };

  // output/Bolson.Core/index.js
  var Local = /* @__PURE__ */ function() {
    function Local2(value0) {
      this.value0 = value0;
    }
    ;
    Local2.create = function(value0) {
      return new Local2(value0);
    };
    return Local2;
  }();
  var Global = /* @__PURE__ */ function() {
    function Global2() {
    }
    ;
    Global2.value = new Global2();
    return Global2;
  }();
  var Insert = /* @__PURE__ */ function() {
    function Insert2(value0) {
      this.value0 = value0;
    }
    ;
    Insert2.create = function(value0) {
      return new Insert2(value0);
    };
    return Insert2;
  }();
  var Remove = /* @__PURE__ */ function() {
    function Remove2() {
    }
    ;
    Remove2.value = new Remove2();
    return Remove2;
  }();
  var Logic = /* @__PURE__ */ function() {
    function Logic2(value0) {
      this.value0 = value0;
    }
    ;
    Logic2.create = function(value0) {
      return new Logic2(value0);
    };
    return Logic2;
  }();
  var DynamicChildren$prime = /* @__PURE__ */ function() {
    function DynamicChildren$prime2(value0) {
      this.value0 = value0;
    }
    ;
    DynamicChildren$prime2.create = function(value0) {
      return new DynamicChildren$prime2(value0);
    };
    return DynamicChildren$prime2;
  }();
  var FixedChildren$prime = /* @__PURE__ */ function() {
    function FixedChildren$prime2(value0) {
      this.value0 = value0;
    }
    ;
    FixedChildren$prime2.create = function(value0) {
      return new FixedChildren$prime2(value0);
    };
    return FixedChildren$prime2;
  }();
  var EventfulElement$prime = /* @__PURE__ */ function() {
    function EventfulElement$prime2(value0) {
      this.value0 = value0;
    }
    ;
    EventfulElement$prime2.create = function(value0) {
      return new EventfulElement$prime2(value0);
    };
    return EventfulElement$prime2;
  }();
  var Element$prime = /* @__PURE__ */ function() {
    function Element$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Element$prime2.create = function(value0) {
      return new Element$prime2(value0);
    };
    return Element$prime2;
  }();
  var eqScope = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Local && y instanceof Local) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Global && y instanceof Global) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var fixed = function(a) {
    return new FixedChildren$prime(a);
  };
  var envy = function(a) {
    return new EventfulElement$prime(a);
  };
  var dyn = function(a) {
    return new DynamicChildren$prime(a);
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply5) {
      return function(map14) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply5(map14(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last2 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last2, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map14(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map14(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty4 = {};
  function runST(f) {
    return f();
  }
  function _foldM(bind5) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind5(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object.ST/foreign.js
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }
  var deleteImpl = function(k) {
    return function(m) {
      return function() {
        delete m[k];
        return m;
      };
    };
  };

  // output/Foreign.Object/index.js
  var foldr3 = /* @__PURE__ */ foldr(foldableArray);
  var values = /* @__PURE__ */ toArrayWithKey(function(v) {
    return function(v1) {
      return v1;
    };
  });
  var thawST = _copyST;
  var mutate = function(f) {
    return function(m) {
      return runST(function __do4() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var insert5 = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var fold3 = /* @__PURE__ */ _foldM(applyFlipped);
  var foldMap3 = function(dictMonoid) {
    var append12 = append(dictMonoid.Semigroup0());
    var mempty4 = mempty(dictMonoid);
    return function(f) {
      return fold3(function(acc) {
        return function(k) {
          return function(v) {
            return append12(acc)(f(k)(v));
          };
        };
      })(mempty4);
    };
  };
  var foldableObject = {
    foldl: function(f) {
      return fold3(function(z) {
        return function(v) {
          return f(z);
        };
      });
    },
    foldr: function(f) {
      return function(z) {
        return function(m) {
          return foldr3(f)(z)(values(m));
        };
      };
    },
    foldMap: function(dictMonoid) {
      var foldMap12 = foldMap3(dictMonoid);
      return function(f) {
        return foldMap12($$const(f));
      };
    }
  };
  var $$delete5 = function(k) {
    return mutate(deleteImpl(k));
  };

  // output/Bolson.Control/index.js
  var keepLatest3 = /* @__PURE__ */ keepLatest(eventIsEvent);
  var map5 = /* @__PURE__ */ map(functorEvent);
  var bind2 = /* @__PURE__ */ bind(bindST);
  var pure1 = /* @__PURE__ */ pure(applicativeST);
  var map22 = /* @__PURE__ */ map(functorST);
  var for_3 = /* @__PURE__ */ for_(applicativeST);
  var for_12 = /* @__PURE__ */ for_3(foldableMaybe);
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var for_23 = /* @__PURE__ */ for_3(foldableArray);
  var oneOfMap2 = /* @__PURE__ */ oneOfMap(foldableArray)(plusEvent);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeST)(foldableArray);
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var foldl3 = /* @__PURE__ */ foldl(foldableObject);
  var applySecond2 = /* @__PURE__ */ applySecond(applyST);
  var Begin = /* @__PURE__ */ function() {
    function Begin2() {
    }
    ;
    Begin2.value = new Begin2();
    return Begin2;
  }();
  var Middle = /* @__PURE__ */ function() {
    function Middle2() {
    }
    ;
    Middle2.value = new Middle2();
    return Middle2;
  }();
  var End = /* @__PURE__ */ function() {
    function End2() {
    }
    ;
    End2.value = new End2();
    return End2;
  }();
  var flatten = function(v) {
    return function(psr) {
      return function(interpreter) {
        var element = function(v1) {
          return v1(psr)(interpreter);
        };
        return function(v1) {
          if (v1 instanceof FixedChildren$prime) {
            return oneOfMap2(flatten(v)(psr)(interpreter))(v1.value0);
          }
          ;
          if (v1 instanceof EventfulElement$prime) {
            return keepLatest3(map5(flatten(v)(psr)(interpreter))(v1.value0));
          }
          ;
          if (v1 instanceof Element$prime) {
            return element(v.toElt(v1.value0));
          }
          ;
          if (v1 instanceof DynamicChildren$prime) {
            return makeLemmingEventO(function(v2, v3) {
              var cancelInner = newSTRef(empty4)();
              var cancelOuter = v2(v1.value0, function(inner) {
                var myUnsubId = v.ids(interpreter)();
                var myUnsub = newSTRef(pure1(unit))();
                var eltsUnsubId = v.ids(interpreter)();
                var eltsUnsub = newSTRef(pure1(unit))();
                var myIds = newSTRef([])();
                var myImmediateCancellation = newSTRef(pure1(unit))();
                var myScope = map22(Local.create)(v.ids(interpreter))();
                var stageRef = newSTRef(Begin.value)();
                var c0 = v2(inner, function(kid$prime) {
                  var stage = read2(stageRef)();
                  if (kid$prime instanceof Logic && stage instanceof Middle) {
                    var curId = read2(myIds)();
                    return traverse_2(function(i) {
                      return function() {
                        return v3(v.doLogic(kid$prime.value0)(interpreter)(i));
                      };
                    })(curId)();
                  }
                  ;
                  if (kid$prime instanceof Remove && stage instanceof Middle) {
                    $$void4(write2(End.value)(stageRef))();
                    var mic = function __do4() {
                      var idRef = read2(myIds)();
                      for_23(idRef)(function(old) {
                        return for_12(psr.parent)(function(pnt) {
                          return function() {
                            return v3(v.disconnectElement(interpreter)({
                              id: old,
                              parent: pnt,
                              scope: myScope
                            }));
                          };
                        });
                      })();
                      var myu = read2(myUnsub)();
                      myu();
                      var eltu = read2(eltsUnsub)();
                      eltu();
                      $$void4(modify2($$delete5(myUnsubId))(cancelInner))();
                      return $$void4(modify2($$delete5(eltsUnsubId))(cancelInner))();
                    };
                    $$void4(write2(mic)(myImmediateCancellation))();
                    return mic();
                  }
                  ;
                  if (kid$prime instanceof Insert && stage instanceof Begin) {
                    $$void4(write2(Middle.value)(stageRef))();
                    var c1 = v2(flatten(v)(function() {
                      var $125 = {};
                      for (var $126 in psr) {
                        if ({}.hasOwnProperty.call(psr, $126)) {
                          $125[$126] = psr[$126];
                        }
                        ;
                      }
                      ;
                      $125.scope = myScope;
                      $125.raiseId = function(id) {
                        return $$void4(modify2(append3([id]))(myIds));
                      };
                      return $125;
                    }())(interpreter)(kid$prime.value0), v3);
                    $$void4(modify2(insert5(eltsUnsubId)(c1))(cancelInner))();
                    return $$void4(write2(c1)(eltsUnsub))();
                  }
                  ;
                  return unit;
                });
                $$void4(write2(c0)(myUnsub))();
                $$void4(modify2(insert5(myUnsubId)(c0))(cancelInner))();
                var mican = read2(myImmediateCancellation)();
                return mican();
              });
              return function __do4() {
                bind2(read2(cancelInner))(foldl3(applySecond2)(pure1(unit)))();
                return cancelOuter();
              };
            });
          }
          ;
          throw new Error("Failed pattern match at Bolson.Control (line 544, column 17 - line 630, column 20): " + [v1.constructor.name]);
        };
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };

  // output/Deku.Core/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var lcmap2 = /* @__PURE__ */ lcmap(profunctorFn);
  var map6 = /* @__PURE__ */ map(functorEvent);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var eq2 = /* @__PURE__ */ eq(eqScope);
  var pure5 = /* @__PURE__ */ pure(applicativeST);
  var pure12 = /* @__PURE__ */ pure(applicativeEvent);
  var empty5 = /* @__PURE__ */ empty(plusEvent);
  var oneOf2 = /* @__PURE__ */ oneOf(foldableArray)(plusEvent);
  var unsafeSetPos$prime = function(i) {
    return function(v) {
      var f = function(v1) {
        if (v1 instanceof Element$prime) {
          return new Element$prime(lcmap2(function(v2) {
            return {
              pos: i,
              dynFamily: v2.dynFamily,
              ez: v2.ez,
              parent: v2.parent,
              raiseId: v2.raiseId,
              scope: v2.scope
            };
          })(v1.value0));
        }
        ;
        if (v1 instanceof EventfulElement$prime) {
          return new EventfulElement$prime(map6(f)(v1.value0));
        }
        ;
        return v;
      };
      return f(v);
    };
  };
  var unsafeSetPos = function($77) {
    return unsafeSetPos$prime(Just.create($77));
  };
  var portalFlatten = function() {
    return {
      doLogic: function(pos) {
        return function(v) {
          return function(id) {
            return v.sendToPos({
              id,
              pos
            });
          };
        };
      },
      ids: function($78) {
        return function(v) {
          return v.ids;
        }(unwrap2($78));
      },
      disconnectElement: function(v) {
        return function(v1) {
          return v.disconnectElement({
            id: v1.id,
            scope: v1.scope,
            parent: v1.parent,
            scopeEq: eq2
          });
        };
      },
      toElt: function(v) {
        return v;
      }
    };
  };
  var portalFlatten1 = /* @__PURE__ */ portalFlatten();
  var insert_ = function(d) {
    return new Insert(unwrap2(unsafeSetPos$prime(Nothing.value)(d)));
  };
  var bus2 = function(f) {
    return bus(f);
  };
  var bussed = function(f) {
    return new EventfulElement$prime(coerce3(bus2(f)));
  };
  var bussedUncurried = function($80) {
    return bussed(curry($80));
  };
  var __internalDekuFlatten = function(a) {
    return function(b) {
      return function(v) {
        return flatten(portalFlatten1)(a)(b)(v);
      };
    };
  };
  var dynify = function(f) {
    return function(es) {
      var go2 = function(v) {
        return function(v1) {
          return makeLemmingEventO(function(v2, k) {
            var me = v1.ids();
            v.raiseId(me)();
            var v3 = function() {
              if (v.parent instanceof Nothing) {
                var dummyParent = v1.ids();
                return new Tuple(pure12(v1.makeElement({
                  id: dummyParent,
                  parent: Nothing.value,
                  scope: v.scope,
                  tag: "div",
                  pos: Nothing.value,
                  dynFamily: Nothing.value
                })), dummyParent);
              }
              ;
              if (v.parent instanceof Just) {
                return new Tuple(empty5, v.parent.value0);
              }
              ;
              throw new Error("Failed pattern match at Deku.Core (line 408, column 34 - line 422, column 36): " + [v.parent.constructor.name]);
            }();
            var unsub = v2(oneOf2([v3.value0, pure12(v1.makeDynBeacon({
              id: me,
              parent: new Just(v3.value1),
              scope: v.scope,
              dynFamily: v.dynFamily,
              pos: v.pos
            })), pure12(v1.attributeParent({
              id: me,
              parent: v3.value1,
              pos: v.pos,
              dynFamily: v.dynFamily,
              ez: v.ez
            })), __internalDekuFlatten({
              parent: new Just(v3.value1),
              scope: v.scope,
              ez: false,
              raiseId: function(v4) {
                return pure5(unit);
              },
              pos: Nothing.value,
              dynFamily: new Just(me)
            })(v1)(f(es))]), k);
            return function __do4() {
              k(v1.removeDynBeacon({
                id: me
              }));
              return unsub();
            };
          });
        };
      };
      return new Element$prime(go2);
    };
  };
  var dyn2 = /* @__PURE__ */ dynify(/* @__PURE__ */ coerce3(dyn));

  // output/Deku.Control/index.js
  var map7 = /* @__PURE__ */ map(functorEvent);
  var oneOf3 = /* @__PURE__ */ oneOf(foldableArray)(plusEvent);
  var pure6 = /* @__PURE__ */ pure(applicativeEvent);
  var empty6 = /* @__PURE__ */ empty(plusEvent);
  var pure13 = /* @__PURE__ */ pure(applicativeST);
  var mapAccum2 = /* @__PURE__ */ mapAccum(eventIsEvent);
  var keepLatest4 = /* @__PURE__ */ keepLatest(eventIsEvent);
  var filter6 = /* @__PURE__ */ filter4(filterableEvent);
  var eq3 = /* @__PURE__ */ eq(eqInt);
  var coerce4 = /* @__PURE__ */ coerce();
  var unwrap3 = /* @__PURE__ */ unwrap();
  var eq12 = /* @__PURE__ */ eq(eqScope);
  var alt2 = /* @__PURE__ */ alt(altEvent);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var unsafeSetAttribute = function(v) {
    return function(id) {
      return function(atts) {
        return map7(function($132) {
          return function(v1) {
            if (v1.value instanceof Prop$prime) {
              return v.setProp({
                id,
                key: v1.key,
                value: v1.value.value0
              });
            }
            ;
            if (v1.value instanceof Cb$prime) {
              return v.setCb({
                id,
                key: v1.key,
                value: v1.value.value0
              });
            }
            ;
            if (v1.value instanceof Unset$prime) {
              return v.unsetAttribute({
                id,
                key: v1.key
              });
            }
            ;
            throw new Error("Failed pattern match at Deku.Control (line 71, column 28 - line 74, column 47): " + [v1.value.constructor.name]);
          }(unsafeUnAttribute($132));
        })(atts);
      };
    };
  };
  var switcher = function(f) {
    return function(event) {
      var counter = function() {
        var fn = function(a) {
          return function(b) {
            return new Tuple(a + 1 | 0, new Tuple(b, a));
          };
        };
        return mapAccum2(fn)(0);
      }();
      return dyn2(keepLatest4(memoize(counter(event))(function(cenv) {
        return map7(function(v) {
          return oneOf3([map7($$const(Remove.value))(filter6(function() {
            var $133 = eq3(v.value1 + 1 | 0);
            return function($134) {
              return $133(snd($134));
            };
          }())(cenv)), pure6(insert_(coerce4(f(v.value0))))]);
        })(cenv);
      })));
    };
  };
  var switcherFlipped = /* @__PURE__ */ flip(switcher);
  var portalFlatten2 = function() {
    return {
      doLogic: function(pos) {
        return function(v) {
          return function(id) {
            return v.sendToPos({
              id,
              pos
            });
          };
        };
      },
      ids: function($135) {
        return function(v) {
          return v.ids;
        }(unwrap3($135));
      },
      disconnectElement: function(v) {
        return function(v1) {
          return v.disconnectElement({
            id: v1.id,
            scope: v1.scope,
            parent: v1.parent,
            scopeEq: eq12
          });
        };
      },
      toElt: function(v) {
        return v;
      }
    };
  };
  var portalFlatten12 = /* @__PURE__ */ portalFlatten2();
  var blank = /* @__PURE__ */ envy(empty6);
  var __internalDekuFlatten2 = function(a) {
    return function(b) {
      return function(v) {
        return flatten(portalFlatten12)(a)(b)(v);
      };
    };
  };
  var deku = function(root) {
    return function(children) {
      return function(v) {
        return makeLemmingEventO(function(v1, k) {
          return v1(alt2(pure6(v.makeRoot({
            id: "deku-root",
            root
          })))(__internalDekuFlatten2({
            parent: new Just("deku-root"),
            scope: new Local("rootScope"),
            raiseId: function(v2) {
              return pure13(unit);
            },
            ez: true,
            pos: Nothing.value,
            dynFamily: Nothing.value
          })(v)(children)), k);
        });
      };
    };
  };
  var elementify = function(tag) {
    return function(atts) {
      return function(children) {
        var go2 = function(v) {
          return function(v1) {
            return makeLemmingEventO(function(v2, k) {
              var me = v1.ids();
              v.raiseId(me)();
              var unsub = v2(alt2(oneOf3(append4([pure6(v1.makeElement({
                id: me,
                parent: v.parent,
                scope: v.scope,
                tag,
                pos: v.pos,
                dynFamily: v.dynFamily
              })), unsafeSetAttribute(v1)(me)(atts)])(maybe([])(function(p) {
                return [pure6(v1.attributeParent({
                  id: me,
                  parent: p,
                  pos: v.pos,
                  dynFamily: v.dynFamily,
                  ez: v.ez
                }))];
              })(v.parent))))(__internalDekuFlatten2({
                parent: new Just(me),
                scope: v.scope,
                ez: true,
                raiseId: function(v3) {
                  return pure13(unit);
                },
                pos: Nothing.value,
                dynFamily: Nothing.value
              })(v1)(children)), k);
              return function __do4() {
                k(v1.deleteFromCache({
                  id: me
                }));
                return unsub();
              };
            });
          };
        };
        return go2;
      };
    };
  };

  // output/Deku.DOM.Attr.Fill/index.js
  var Fill = /* @__PURE__ */ function() {
    function Fill2() {
    }
    ;
    Fill2.value = new Fill2();
    return Fill2;
  }();
  var attrRect_FillString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "fill",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Height/index.js
  var Height = /* @__PURE__ */ function() {
    function Height2() {
    }
    ;
    Height2.value = new Height2();
    return Height2;
  }();
  var attrSvg_HeightString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "height",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrRect_HeightString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "height",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrPattern_HeightString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "height",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrImage_HeightString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "height",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Href/index.js
  var Href = /* @__PURE__ */ function() {
    function Href2() {
    }
    ;
    Href2.value = new Href2();
    return Href2;
  }();
  var attrImage_HrefString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "href",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Id/index.js
  var Id = /* @__PURE__ */ function() {
    function Id2() {
    }
    ;
    Id2.value = new Id2();
    return Id2;
  }();
  var attrPattern_IdString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "id",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.OnMousedown/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorEffect);
  var OnMousedown = /* @__PURE__ */ function() {
    function OnMousedown2() {
    }
    ;
    OnMousedown2.value = new OnMousedown2();
    return OnMousedown2;
  }();
  var attrOnMousedownEffectUnit = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "mousedown",
          value: cb$prime($$const(voidLeft2(value12)(true)))
        });
      };
    }
  };

  // output/Deku.DOM.Attr.OnMouseup/index.js
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var OnMouseup = /* @__PURE__ */ function() {
    function OnMouseup2() {
    }
    ;
    OnMouseup2.value = new OnMouseup2();
    return OnMouseup2;
  }();
  var attrOnMouseupEffectUnit = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "mouseup",
          value: cb$prime($$const(voidLeft3(value12)(true)))
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Stroke/index.js
  var Stroke = /* @__PURE__ */ function() {
    function Stroke2() {
    }
    ;
    Stroke2.value = new Stroke2();
    return Stroke2;
  }();
  var attrRect_StrokeString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "stroke",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.StrokeWidth/index.js
  var StrokeWidth = /* @__PURE__ */ function() {
    function StrokeWidth2() {
    }
    ;
    StrokeWidth2.value = new StrokeWidth2();
    return StrokeWidth2;
  }();
  var attrRect_StrokeWidthStrin = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "stroke-width",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.ViewBox/index.js
  var ViewBox = /* @__PURE__ */ function() {
    function ViewBox2() {
    }
    ;
    ViewBox2.value = new ViewBox2();
    return ViewBox2;
  }();
  var attrPattern_ViewBoxString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "viewBox",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Width/index.js
  var Width = /* @__PURE__ */ function() {
    function Width2() {
    }
    ;
    Width2.value = new Width2();
    return Width2;
  }();
  var attrSvg_WidthString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "width",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrRect_WidthString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "width",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrPattern_WidthString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "width",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrImage_WidthString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "width",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.X/index.js
  var X = /* @__PURE__ */ function() {
    function X2() {
    }
    ;
    X2.value = new X2();
    return X2;
  }();
  var attrRect_XString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "x",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrImage_XString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "x",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Y/index.js
  var Y = /* @__PURE__ */ function() {
    function Y2() {
    }
    ;
    Y2.value = new Y2();
    return Y2;
  }();
  var attrRect_YString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "y",
          value: prop$prime(value12)
        });
      };
    }
  };
  var attrImage_YString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "y",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Elt.Defs/index.js
  var coerce5 = /* @__PURE__ */ coerce();
  var defs = function(attributes) {
    return function(kids) {
      return new Element$prime(elementify("defs")(attributes)(coerce5(fixed(coerce5(mapWithIndex2(unsafeSetPos)(kids))))));
    };
  };
  var defs_ = /* @__PURE__ */ defs(/* @__PURE__ */ empty(plusEvent));

  // output/Deku.DOM.Elt.Image/index.js
  var coerce6 = /* @__PURE__ */ coerce();
  var image = function(attributes) {
    return function(kids) {
      return new Element$prime(elementify("image")(attributes)(coerce6(fixed(coerce6(mapWithIndex2(unsafeSetPos)(kids))))));
    };
  };

  // output/Deku.DOM.Elt.Pattern/index.js
  var coerce7 = /* @__PURE__ */ coerce();
  var pattern = function(attributes) {
    return function(kids) {
      return new Element$prime(elementify("pattern")(attributes)(coerce7(fixed(coerce7(mapWithIndex2(unsafeSetPos)(kids))))));
    };
  };

  // output/Deku.DOM.Elt.Rect/index.js
  var coerce8 = /* @__PURE__ */ coerce();
  var rect = function(attributes) {
    return function(kids) {
      return new Element$prime(elementify("rect")(attributes)(coerce8(fixed(coerce8(mapWithIndex2(unsafeSetPos)(kids))))));
    };
  };

  // output/Deku.DOM.Elt.Svg/index.js
  var coerce9 = /* @__PURE__ */ coerce();
  var svg = function(attributes) {
    return function(kids) {
      return new Element$prime(elementify("svg")(attributes)(coerce9(fixed(coerce9(mapWithIndex2(unsafeSetPos)(kids))))));
    };
  };

  // output/Deku.Do/index.js
  var bind3 = function(f) {
    return function(a) {
      return f(a);
    };
  };

  // output/Deku.Hooks/index.js
  var coerce10 = /* @__PURE__ */ coerce();
  var alt3 = /* @__PURE__ */ alt(altEvent);
  var pure7 = /* @__PURE__ */ pure(applicativeEvent);
  var useState$prime = bussedUncurried;
  var useMemoized = function(e) {
    return function(f1) {
      return envy(coerce10(memoize(e)(f1)));
    };
  };
  var useState = function(a) {
    return function(f) {
      return bind3(useState$prime)(function(v) {
        return bind3(useMemoized(alt3(v.value1)(pure7(a))))(function(m) {
          return f(new Tuple(v.value0, m));
        });
      });
    };
  };
  var useMailboxed = function(dictOrd) {
    var mailboxed2 = mailboxed(dictOrd);
    return function(f) {
      return bussedUncurried(function(v) {
        return envy(coerce10(mailboxed2(v.value1)(function(c) {
          return f(new Tuple(v.value0, c));
        })));
      });
    };
  };

  // output/Deku.Interpret/foreign.js
  var attributeParent_ = (runOnJust2) => (a) => (state4) => () => {
    if (state4.units[a.id]) {
      const dom2 = state4.units[a.parent].main;
      if (!(state4.units[a.id].main && state4.units[a.id].main.parentNode || state4.units[a.id].startBeacon && state4.units[a.id].startBeacon.parentNode)) {
        const iRan = a.ez ? (() => {
          if (state4.units[a.id].main) {
            dom2.appendChild(state4.units[a.id].main);
          } else {
            dom2.appendChild(state4.units[a.id].startBeacon);
            dom2.appendChild(state4.units[a.id].endBeacon);
          }
          return true;
        })() : runOnJust2(a.pos)((pos) => () => {
          return runOnJust2(a.dynFamily)((dynFamily) => () => {
            var i = 0;
            var j = 0;
            var terminalDyn;
            while (j < dom2.childNodes.length) {
              if (dom2.childNodes[j].nodeType === 8 && dom2.childNodes[j].nodeValue === "%-%" + dynFamily) {
                j += 1;
                break;
              }
              j++;
            }
            const inserter = (k) => {
              if (state4.units[a.id].startBeacon) {
                dom2.insertBefore(
                  state4.units[a.id].startBeacon,
                  dom2.childNodes[k]
                );
                dom2.insertBefore(
                  state4.units[a.id].endBeacon,
                  dom2.childNodes[k]
                );
              } else {
                dom2.insertBefore(state4.units[a.id].main, dom2.childNodes[k]);
              }
            };
            while (j < dom2.childNodes.length) {
              var tmpDekuId;
              if (tmpDekuId = dom2.childNodes[j].$dekuId) {
                const insertHappened = runOnJust2(
                  state4.units[tmpDekuId].dynFamily
                )((tmpDynFamily) => () => {
                  const insertHappened2 = runOnJust2(
                    state4.units[tmpDekuId].pos
                  )((tmpPos) => () => {
                    if (dynFamily === tmpDynFamily && pos <= tmpPos) {
                      inserter(j);
                      return true;
                    }
                    return false;
                  })();
                  return insertHappened2;
                })();
                if (insertHappened) {
                  return true;
                }
              }
              if (i === pos) {
                inserter(j);
                return true;
              }
              if (dom2.childNodes[j].nodeType === 8 && dom2.childNodes[j].nodeValue === "%-%" + dynFamily + "%-%") {
                inserter(j);
                return true;
              }
              if (dom2.childNodes[j].nodeType === 8 && dom2.childNodes[j].nodeValue.substring(0, 3) === "%-%" && !terminalDyn) {
                terminalDyn = dom2.childNodes[j].nodeValue + "%-%";
              }
              if (!terminalDyn) {
                i++;
              }
              if (dom2.childNodes[j].nodeType === 8 && dom2.childNodes[j].nodeValue === terminalDyn) {
                terminalDyn = void 0;
                i++;
              }
              j++;
            }
            return false;
          })();
        })();
        if (!iRan) {
          if (a.parent.indexOf("@!%") !== -1) {
            const usedDynBeacon = runOnJust2(a.dynFamily)((df) => () => {
              if (state4.units[a.id].main) {
                state4.units[df].endBeacon.parentNode.insertBefore(
                  state4.units[a.id].main,
                  state4.units[df].endBeacon
                );
              } else {
                state4.units[df].endBeacon.parentNode.insertBefore(
                  state4.units[a.id].endBeacon,
                  state4.units[df].endBeacon
                );
                state4.units[df].endBeacon.parentNode.insertBefore(
                  state4.units[a.id].startBeacon,
                  state4.units[a.id].endBeacon
                );
              }
              return true;
            })();
            if (usedDynBeacon) {
            } else if (state4.units[a.id].main) {
              dom2.parentNode.replaceChild(state4.units[a.id].main, dom2);
            } else {
              dom2.parentNode.replaceChild(state4.units[a.id].endBeacon, dom2);
              state4.units[a.id].endBeacon.parentNode.insertBefore(
                state4.units[a.id].startBeacon,
                state4.units[a.id].endBeacon
              );
            }
          } else {
            const hasADynFamily = runOnJust2(a.dynFamily)((dynFamily) => () => {
              if (state4.units[a.id].startBeacon) {
                dom2.insertBefore(
                  state4.units[a.id].startBeacon,
                  state4.units[dynFamily].endBeacon
                );
                dom2.insertBefore(
                  state4.units[a.id].endBeacon,
                  state4.units[dynFamily].endBeacon
                );
              } else {
                dom2.insertBefore(
                  state4.units[a.id].main,
                  state4.units[dynFamily].endBeacon
                );
              }
              return true;
            })();
            if (!hasADynFamily) {
              if (state4.units[a.id].startBeacon) {
                dom2.appendChild(state4.units[a.id].startBeacon);
                dom2.appendChild(state4.units[a.id].endBeacon);
              } else {
                dom2.appendChild(state4.units[a.id].main);
              }
            }
          }
        }
      }
    }
  };
  var makeDynBeacon_ = (runOnJust2) => (tryHydration) => (a) => (state4) => () => {
    var startBeacon;
    var endBeacon;
    var ptr = a.id;
    if (!state4.scopes[a.scope]) {
      state4.scopes[a.scope] = [];
    }
    state4.scopes[a.scope].push(ptr);
    const iRan = runOnJust2(a.parent)(() => () => {
      if (state4.hydrating && tryHydration && (startBeacon = state4.allBeacons[a.id]) && (endBeacon = state4.allBeacons[`${a.id}%-%`])) {
        state4.units[ptr] = {
          listeners: {},
          parent: a.parent,
          scope: a.scope,
          pos: a.pos,
          dynFamily: a.dynFamily,
          startBeacon,
          endBeacon
        };
        startBeacon.$dekuId = ptr;
        endBeacon.$dekuId = ptr;
        return true;
      }
      return false;
    })();
    if (!iRan) {
      const startBeacon2 = document.createComment(`%-%${a.id}`);
      const endBeacon2 = document.createComment(`%-%${a.id}%-%`);
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        dynFamily: a.dynFamily,
        scope: a.scope,
        pos: a.pos,
        startBeacon: startBeacon2,
        endBeacon: endBeacon2
      };
      startBeacon2.$dekuId = ptr;
      endBeacon2.$dekuId = ptr;
    }
  };
  var svgTags = /* @__PURE__ */ new Set([
    "animate",
    "animateMotion",
    "animateTransform",
    "circle",
    "clipPath",
    "defs",
    "desc",
    "discard",
    "ellipse",
    "feBlend",
    "feColorMatrix",
    "feComponentTransfer",
    "feComposite",
    "feConvolveMatrix",
    "feDiffuseLighting",
    "feDisplacementMap",
    "feDistantLight",
    "feDropShadow",
    "feFlood",
    "feFuncA",
    "feFuncB",
    "feFuncG",
    "feFuncR",
    "feGaussianBlur",
    "feImage",
    "feMerge",
    "feMergeNode",
    "feMorphology",
    "feOffset",
    "fePointLight",
    "feSpecularLighting",
    "feSpotLight",
    "feTile",
    "feTurbulence",
    "filter",
    "foreignObject",
    "g",
    "image",
    "line",
    "linearGradient",
    "marker",
    "mask",
    "metadata",
    "mpath",
    "path",
    "pattern",
    "polygon",
    "polyline",
    "radialGradient",
    "rect",
    "set",
    "stop",
    "svg",
    "switch",
    "symbol",
    "text",
    "textPath",
    "title",
    "tspan",
    "use",
    "view"
  ]);
  var getDynFamily = (id) => (state4) => () => state4.units[id] && state4.units[id].dynFamily ? state4.units[id].dynFamily : (() => {
    throw new Error(`No positional information for ${id}`);
  })();
  var getParent = (id) => (state4) => () => state4.units[id] && state4.units[id].main && state4.units[id].main.parentNode && state4.units[id].main.parentNode.$dekuId ? state4.units[id].main.parentNode.$dekuId : state4.units[id] && state4.units[id].startBeacon && state4.units[id].startBeacon.parentNode && state4.units[id].startBeacon.parentNode.$dekuId ? state4.units[id].startBeacon.parentNode.$dekuId : (() => {
    throw new Error(`No parent information for ${id}`);
  })();
  var getScope = (id) => (state4) => () => state4.units[id] && state4.units[id].scope ? state4.units[id].scope : (() => {
    throw new Error(`No scope information for ${id}`);
  })();
  var makeElement_ = (runOnJust2) => (tryHydration) => (a) => (state4) => () => {
    var dom2;
    var ptr = a.id;
    if (!state4.scopes[a.scope]) {
      state4.scopes[a.scope] = [];
    }
    state4.scopes[a.scope].push(ptr);
    const iRan = runOnJust2(a.parent)(() => () => {
      if (state4.hydrating && tryHydration && (dom2 = document.documentElement.querySelector(
        `[data-deku-ssr="${ptr}"]`
      ))) {
        state4.units[ptr] = {
          listeners: {},
          pos: a.pos,
          parent: a.parent,
          scope: a.scope,
          dynFamily: a.dynFamily,
          main: dom2
        };
        dom2.$dekuId = ptr;
        return true;
      }
      return false;
    })();
    if (!iRan) {
      const main2 = svgTags.has(a.tag) ? document.createElementNS("http://www.w3.org/2000/svg", a.tag) : document.createElement(a.tag);
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        pos: a.pos,
        scope: a.scope,
        dynFamily: a.dynFamily,
        main: main2
      };
      main2.$dekuId = ptr;
    }
  };
  var makeText_ = (runOnJust2) => (tryHydration) => (maybe2) => (a) => (state4) => () => {
    var ptr = a.id;
    var dom2;
    if (!state4.scopes[a.scope]) {
      state4.scopes[a.scope] = [];
    }
    state4.scopes[a.scope].push(ptr);
    const iRan = runOnJust2(a.parent)((parent2) => () => {
      if (state4.hydrating && tryHydration && // hack
      (dom2 = document.documentElement.querySelector(`[data-deku-ssr="${parent2}"]`))) {
        var i = 0;
        for (; i < dom2.childNodes.length; i++) {
          const ptrSplit = ptr.split("@-@");
          if (dom2.childNodes[i].nodeType === 8 && dom2.childNodes[i].nodeValue === ptrSplit[0]) {
            i = i - 1;
            var textWasBlank = i === -1;
            var textWasBlankAfterDynBeacon = i >= 0 && dom2.childNodes[i].nodeType === 8;
            if (textWasBlank) {
              dom2.prepend(document.createTextNode(""));
            }
            if (textWasBlankAfterDynBeacon) {
              dom2.insertBefore(
                document.createTextNode(""),
                dom2.childNodes[i + 1]
              );
            }
            break;
          }
        }
        const main2 = dom2.childNodes[i];
        state4.units[ptr] = {
          // if we've done ssr for a text node, it will be a span,
          // so we want to get the child node
          main: main2,
          pos: a.pos,
          parent: a.parent,
          scope: a.scope
        };
        main2.$dekuId = ptr;
        return true;
      }
      return false;
    })();
    if (!iRan) {
      const main2 = document.createTextNode("");
      state4.units[ptr] = {
        main: main2,
        parent: a.parent,
        scope: a.scope,
        pos: a.pos,
        dynFamily: a.dynFamily
      };
      main2.$dekuId = ptr;
    }
  };
  function makeFFIDOMSnapshot() {
    return {
      units: {},
      scopes: {},
      allBeacons: {}
    };
  }
  var setProp_ = (tryHydration) => (a) => (state4) => () => {
    if (state4.units[a.id]) {
      var ptr = a.id;
      var avv = a.value;
      if (state4.hydrating && tryHydration && !state4.units[ptr] && (dom = document.documentElement.querySelector(`[data-deku-ssr="${ptr}"]`))) {
        state4.units[ptr] = {
          listeners: {},
          parent: a.parent,
          scope: a.scope,
          main: dom
        };
        if (!state4.scopes[a.scope]) {
          state4.scopes[a.scope] = [];
        }
        state4.scopes[a.scope].push(ptr);
      }
      if (state4.units[ptr].main.tagName === "INPUT" && a.key === "value") {
        state4.units[ptr].main.value = avv;
      } else if (state4.units[ptr].main.tagName === "TEXTAREA" && a.key === "value") {
        state4.units[ptr].main.value = avv;
      } else if (state4.units[ptr].main.tagName === "INPUT" && a.key === "checked") {
        state4.units[ptr].main.checked = avv === "true";
      } else if (a.key === "disabled") {
        state4.units[ptr].main.disabled = avv === "true";
      } else {
        state4.units[ptr].main.setAttribute(a.key, avv);
      }
    }
  };
  var setCb_ = (tryHydration) => (a) => (state4) => () => {
    if (state4.units[a.id]) {
      var ptr = a.id;
      var avv = a.value;
      if (state4.hydrating && tryHydration && !state4.units[ptr] && (dom = document.documentElement.querySelector(`[data-deku-ssr="${ptr}"]`))) {
        state4.units[ptr] = {
          listeners: {},
          parent: a.parent,
          scope: a.scope,
          main: dom
        };
        if (!state4.scopes[a.scope]) {
          state4.scopes[a.scope] = [];
        }
        state4.scopes[a.scope].push(ptr);
      }
      if (a.key === "@self@") {
        avv(state4.units[ptr].main)();
      } else {
        if (state4.units[ptr].listeners[a.key]) {
          state4.units[ptr].main.removeEventListener(
            a.key,
            state4.units[ptr].listeners[a.key]
          );
        }
        var el = (e) => avv(e)();
        state4.units[ptr].main.addEventListener(a.key, el);
        state4.units[ptr].listeners[a.key] = el;
      }
    }
  };
  var unsetAttribute_ = (tryHydration) => (a) => (state4) => () => {
    if (state4.units[a.id]) {
      var ptr = a.id;
      if (state4.hydrating && tryHydration && !state4.units[ptr] && (dom = document.documentElement.querySelector(`[data-deku-ssr="${ptr}"]`))) {
        state4.units[ptr] = {
          listeners: {},
          parent: a.parent,
          scope: a.scope,
          main: dom
        };
        if (!state4.scopes[a.scope]) {
          state4.scopes[a.scope] = [];
        }
        state4.scopes[a.scope].push(ptr);
      }
      state4.units[ptr].main.removeAttribute(a.key);
    }
  };
  var setText_ = (a) => (state4) => () => {
    if (state4.units[a.id]) {
      var ptr = a.id;
      state4.units[ptr].main.nodeValue = a.text;
    }
  };
  var makePursx_ = (runOnJust2) => (tryHydration) => (maybe2) => (a) => (state4) => () => {
    var dom2;
    var tmp;
    var ptr = a.id;
    var html = a.html;
    var verb = a.verb;
    var cache = a.cache;
    var parent2 = a.parent;
    var scope2 = a.scope;
    var pxScope = a.pxScope;
    const iRan = runOnJust2(a.parent)(() => () => {
      if (state4.hydrating && tryHydration && // hack
      (dom2 = document.documentElement.querySelector(
        `[data-deku-ssr="${ptr}"]`
      ))) {
        state4.units[ptr] = {
          listeners: {},
          pos: a.pos,
          scope: scope2,
          parent: parent2,
          main: dom2
        };
        dom2.$dekuId = ptr;
        return true;
      }
      return false;
    })();
    if (!iRan) {
      const entries = Object.entries(cache);
      for (var i = 0; i < entries.length; i++) {
        const key2 = entries[i][0];
        if (entries[i][1] === true) {
          html = html.replace(
            verb + key2 + verb,
            'data-deku-attr-internal="' + key2 + '"'
          );
        } else {
          html = html.replace(
            verb + key2 + verb,
            '<span style="display:contents;" data-deku-elt-internal="' + key2 + '"></span>'
          );
        }
      }
      tmp = document.createElement("div");
      tmp.innerHTML = html.trim();
      state4.units[ptr] = {
        listeners: {},
        pos: a.pos,
        scope: scope2,
        parent: parent2,
        main: tmp.firstChild
      };
      tmp.firstChild.$dekuId = ptr;
    }
    if (!state4.scopes[scope2]) {
      state4.scopes[scope2] = [];
    }
    state4.scopes[scope2].push(ptr);
    if (!tmp) {
      tmp = dom2;
    }
    tmp.querySelectorAll("[data-deku-attr-internal]").forEach(function(e) {
      var key2 = e.getAttribute("data-deku-attr-internal");
      const namespacedKey = key2 + "@!%" + pxScope;
      state4.units[namespacedKey] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state4.scopes[scope2].push(namespacedKey);
    });
    tmp.querySelectorAll("[data-deku-elt-internal]").forEach(function(e) {
      var key2 = e.getAttribute("data-deku-elt-internal");
      const namespacedKey = key2 + "@!%" + pxScope;
      state4.units[key2 + "@!%" + pxScope] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state4.scopes[scope2].push(namespacedKey);
    });
    if (!iRan) {
      state4.units[ptr].main.remove();
    }
  };
  var makeRoot_ = (a) => (state4) => () => {
    var ptr = a.id;
    state4.units[ptr] = {
      main: a.root
    };
    a.root.$dekuId = ptr;
  };
  var giveNewParent_ = (just) => (runOnJust2) => (b) => (state4) => () => {
    const insertAt2 = (ptr, parent2, node) => {
      if (state4.units[ptr].startBeacon) {
        var x2 = state4.units[ptr].startBeacon;
        var y2 = x2.nextSibling;
        state4.units[parent2].main.insertBefore(x2, node);
        x2 = y2;
        while (x2 && x2 !== state4.units[ptr].endBeacon) {
          y2 = x2.nextSibling;
          state4.units[parent2].main.insertBefore(x2, node);
          x2 = y2;
        }
      } else {
        state4.units[parent2].main.insertBefore(state4.units[ptr].main, node);
      }
    };
    const runMe = [];
    runMe.push(b);
    for (var z = 0; z < runMe.length; z++) {
      const a = runMe[z];
      const ptr = a.id;
      const parent2 = a.parent;
      state4.units[ptr].containingScope = a.scope;
      var aPos = void 0;
      runOnJust2(a.pos)((myPos) => () => {
        aPos = myPos;
        return true;
      })();
      if (aPos === void 0) {
        aPos = Number.MAX_VALUE;
      }
      const nodes = state4.units[parent2].main.childNodes;
      var i = 0;
      var didInsert = false;
      var pos = 0;
      while (i < nodes.length) {
        var dkid;
        if (dkid = nodes[i].$dekuId) {
          const insertedBeforeEndBeacon = runOnJust2(a.dynFamily)((df) => () => {
            if (didInsert) {
              return false;
            }
            if (state4.units[dkid].endBeacon === nodes[i] && df === dkid) {
              state4.units[ptr].pos = just(pos);
              insertAt2(ptr, parent2, nodes[i]);
              return true;
            }
            return false;
          })();
          if (insertedBeforeEndBeacon) {
            didInsert = true;
            break;
          }
          if (state4.units[dkid].dynFamily !== state4.units[ptr].dynFamily) {
            i++;
            continue;
          }
          if (didInsert) {
            i++;
            continue;
          }
          if (pos === aPos) {
            insertAt2(ptr, parent2, nodes[i]);
            pos++;
            didInsert = true;
          } else if (state4.units[dkid].endBeacon !== nodes[i]) {
            state4.units[dkid].pos = just(pos);
            pos++;
          }
        }
        i++;
      }
      if (didInsert) {
        return;
      }
      if (state4.units[ptr].main) {
        state4.units[parent2].main.appendChild(state4.units[ptr].main);
      } else {
        var x = state4.units[ptr].startBeacon;
        var y = x.nextSibling;
        state4.units[parent2].main.appendChild(x);
        x = y;
        while (x && x !== state4.units[ptr].endBeacon) {
          y = x.nextSibling;
          state4.units[parent2].main.appendChild(x);
          x = y;
        }
      }
    }
  };
  var disconnectElement_ = (a) => (state4) => () => {
    if (state4.units[a.id]) {
      var ptr = a.id;
      if (state4.units[ptr].containingScope && !a.scopeEq(state4.units[ptr].containingScope)(a.scope)) {
        return;
      }
      if (state4.units[ptr].main) {
        state4.units[ptr].main.remove();
      } else {
        const dummy = document.createElement("div");
        var x = state4.units[ptr].startBeacon;
        var y = x.nextSibling;
        dummy.appendChild(x);
        x = y;
        while (x && x !== state4.units[ptr].endBeacon) {
          y = x.nextSibling;
          dummy.appendChild(x);
          x = y;
        }
        if (x === state4.units[ptr].endBeacon) {
          dummy.appendChild(x);
        }
      }
    }
  };
  var deleteFromCache_ = (a) => (state4) => () => {
    if (state4.units[a.id]) {
      delete state4.units[a.id];
    }
  };
  var removeDynBeacon_ = deleteFromCache_;

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Effect.Random/foreign.js
  var random = Math.random;

  // output/Effect.Random/index.js
  var randomInt = function(low2) {
    return function(high2) {
      return function __do4() {
        var n = random();
        var asNumber = (toNumber(high2) - toNumber(low2) + 1) * n + toNumber(low2);
        return floor2(asNumber);
      };
    };
  };

  // output/Random.LCG/index.js
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unSeed = function(v) {
    return v;
  };
  var seedMin = 1;
  var lcgM = 2147483647;
  var seedMax = /* @__PURE__ */ function() {
    return lcgM - 1 | 0;
  }();
  var mkSeed = function(x) {
    var ensureBetween = function(min5) {
      return function(max6) {
        return function(n) {
          var rangeSize = max6 - min5 | 0;
          var n$prime = mod2(n)(rangeSize);
          var $25 = n$prime < min5;
          if ($25) {
            return n$prime + max6 | 0;
          }
          ;
          return n$prime;
        };
      };
    };
    return ensureBetween(seedMin)(seedMax)(x);
  };
  var lcgC = 0;
  var lcgA = 48271;
  var lcgPerturb = function(d) {
    return function(v) {
      return fromJust3(fromNumber(remainder(toNumber(lcgA) * toNumber(v) + toNumber(d))(toNumber(lcgM))));
    };
  };
  var lcgNext = /* @__PURE__ */ lcgPerturb(lcgC);

  // output/Control.Monad.State.Trans/index.js
  var functorStateT = function(dictFunctor) {
    var map14 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map14(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind5 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind5(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure9 = pure(dictMonad.Applicative0());
    return {
      pure: function(a) {
        return function(s) {
          return pure9(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure9 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure9(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Control.Monad.State/index.js
  var evalState = function(v) {
    return function(s) {
      var v1 = v(s);
      return v1.value0;
    };
  };

  // output/Test.QuickCheck.Gen/index.js
  var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadIdentity);
  var state2 = /* @__PURE__ */ state(monadStateStateT2);
  var functorStateT2 = /* @__PURE__ */ functorStateT(functorIdentity);
  var mul2 = /* @__PURE__ */ mul(semiringNumber);
  var add2 = /* @__PURE__ */ add(semiringNumber);
  var unGen = function(v) {
    return v;
  };
  var lcgStep = /* @__PURE__ */ function() {
    var f = function(s) {
      return new Tuple(unSeed(s.newSeed), function() {
        var $94 = {};
        for (var $95 in s) {
          if ({}.hasOwnProperty.call(s, $95)) {
            $94[$95] = s[$95];
          }
          ;
        }
        ;
        $94.newSeed = lcgNext(s.newSeed);
        return $94;
      }());
    };
    return state2(f);
  }();
  var functorGen = functorStateT2;
  var map23 = /* @__PURE__ */ map(functorGen);
  var evalGen = function($104) {
    return evalState(unGen($104));
  };
  var applyGen = /* @__PURE__ */ applyStateT(monadIdentity);
  var apply4 = /* @__PURE__ */ apply(applyGen);
  var chooseInt$prime = function(a) {
    return function(b) {
      var numB = toNumber(b);
      var numA = toNumber(a);
      var clamp = function(x) {
        return numA + remainder(x)(numB - numA + 1);
      };
      var choose31BitPosNumber = map23(toNumber)(lcgStep);
      var choose32BitPosNumber = apply4(map23(add2)(choose31BitPosNumber))(map23(mul2(2))(choose31BitPosNumber));
      return map23(function($109) {
        return floor2(clamp($109));
      })(choose32BitPosNumber);
    };
  };
  var chooseInt2 = function(a) {
    return function(b) {
      var $101 = a <= b;
      if ($101) {
        return chooseInt$prime(a)(b);
      }
      ;
      return chooseInt$prime(b)(a);
    };
  };

  // output/Test.QuickCheck.Arbitrary/index.js
  var arbitrary = function(dict) {
    return dict.arbitrary;
  };
  var arbInt = /* @__PURE__ */ function() {
    return {
      arbitrary: chooseInt2(-1e6 | 0)(1e6)
    };
  }();

  // output/Deku.Interpret/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorST);
  var show2 = /* @__PURE__ */ show(showInt);
  var arbitrary2 = /* @__PURE__ */ arbitrary(arbInt);
  var add3 = /* @__PURE__ */ add(semiringInt);
  var pure14 = /* @__PURE__ */ pure(applicativeEffect);
  var runOnJust = function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }
      ;
      return pure14(false);
    };
  };
  var sendToPos2 = function(a) {
    return function(state4) {
      return function __do4() {
        var scope2 = getScope(a.id)(state4)();
        var parent2 = getParent(a.id)(state4)();
        var dynFamily = getDynFamily(a.id)(state4)();
        var newA = {
          scope: scope2,
          parent: parent2,
          dynFamily,
          id: a.id,
          pos: new Just(a.pos),
          ez: false
        };
        return giveNewParent_(Just.create)(runOnJust)(newA)(state4)();
      };
    };
  };
  var fullDOMInterpret = function(seed) {
    return {
      ids: function __do4() {
        var s = read2(seed)();
        var o = show2(evalGen(arbitrary2)({
          newSeed: mkSeed(s),
          size: 5
        }));
        $$void5(modify2(add3(1))(seed))();
        return o;
      },
      makeElement: makeElement_(runOnJust)(false),
      makeDynBeacon: makeDynBeacon_(runOnJust)(false),
      attributeParent: attributeParent_(runOnJust),
      makeRoot: makeRoot_,
      makeText: makeText_(runOnJust)(false)(maybe(unit)),
      makePursx: makePursx_(runOnJust)(false)(maybe(unit)),
      setProp: setProp_(false),
      setCb: setCb_(false),
      unsetAttribute: unsetAttribute_(false),
      setText: setText_,
      sendToPos: sendToPos2,
      removeDynBeacon: removeDynBeacon_,
      deleteFromCache: deleteFromCache_,
      giveNewParent: giveNewParent_(Just.create)(runOnJust),
      disconnectElement: disconnectElement_
    };
  };

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name15, value12) {
    if (typeof window !== "undefined") {
      var ty = window[name15];
      if (ty != null && value12 instanceof ty) {
        return just(value12);
      }
    }
    var obj = value12;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name15) {
        return just(value12);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name15) {
    return function(value12) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
    };
  };

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _body(doc) {
    return doc.body;
  }

  // output/Web.HTML.HTMLDocument/index.js
  var map8 = /* @__PURE__ */ map(functorEffect);
  var body = function(doc) {
    return map8(toMaybe)(function() {
      return _body(doc);
    });
  };

  // output/Web.HTML.HTMLElement/index.js
  var toElement = unsafeCoerce2;

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }
  function requestAnimationFrame(fn) {
    return function(window2) {
      return function() {
        return window2.requestAnimationFrame(fn);
      };
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Deku.Toplevel/index.js
  var bind4 = /* @__PURE__ */ bind(bindEffect);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorEffect);
  var liftST3 = /* @__PURE__ */ liftST(monadSTEffect);
  var mempty3 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidEffect(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var map9 = /* @__PURE__ */ map(functorMaybe);
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
  var runInElement$prime = function(elt) {
    return function(eee) {
      return function __do4() {
        var ffi = makeFFIDOMSnapshot();
        var evt = mapFlipped2(liftST3(newSTRef(0)))(function() {
          var $39 = deku(elt)(eee);
          return function($40) {
            return $39(fullDOMInterpret($40));
          };
        }())();
        return subscribe(evt)(function(i) {
          return i(ffi);
        })();
      };
    };
  };
  var runInBody$prime = function(eee) {
    return function __do4() {
      var b$prime = bind4(bind4(windowImpl)(document2))(body)();
      return maybe(mempty3)(function(elt) {
        return runInElement$prime(elt)(eee);
      })(map9(toElement)(b$prime))();
    };
  };
  var runInBody = function(a) {
    return $$void6(runInBody$prime(a));
  };

  // output/FRP.Event.AnimationFrame/index.js
  var $runtime_lazy4 = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var animationFrame = /* @__PURE__ */ makeEvent(function(k) {
    return function __do4() {
      var w = windowImpl();
      var cancelled = $$new(false)();
      var $lazy_loop = $runtime_lazy4("loop", "FRP.Event.AnimationFrame", function() {
        return $$void7(requestAnimationFrame(function __do5() {
          k(unit)();
          return unlessM2(read(cancelled))($lazy_loop(20))();
        })(w));
      });
      var loop2 = $lazy_loop(17);
      loop2();
      return write(true)(cancelled);
    };
  });

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target5) {
          return function() {
            return target5.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target5) {
          return function() {
            return target5.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.UIEvent.KeyboardEvent/foreign.js
  function code(e) {
    return e.code;
  }

  // output/Web.UIEvent.KeyboardEvent/index.js
  var fromEvent = /* @__PURE__ */ unsafeReadProtoTagged("KeyboardEvent");

  // output/FRP.Event.Keyboard/index.js
  var map10 = /* @__PURE__ */ map(functorEffect);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var wrap3 = /* @__PURE__ */ wrap();
  var up = /* @__PURE__ */ makeEvent(function(k) {
    return function __do4() {
      var target5 = map10(toEventTarget)(windowImpl)();
      var keyUpListener = eventListener(function(e) {
        return traverse_3(function(ke) {
          return k(code(ke));
        })(fromEvent(e));
      })();
      addEventListener(wrap3("keyup"))(keyUpListener)(false)(target5)();
      return removeEventListener(wrap3("keyup"))(keyUpListener)(false)(target5);
    };
  });
  var down = /* @__PURE__ */ makeEvent(function(k) {
    return function __do4() {
      var target5 = map10(toEventTarget)(windowImpl)();
      var keyDownListener = eventListener(function(e) {
        return traverse_3(function(ke) {
          return k(code(ke));
        })(fromEvent(e));
      })();
      addEventListener(wrap3("keydown"))(keyDownListener)(false)(target5)();
      return removeEventListener(wrap3("keydown"))(keyDownListener)(false)(target5);
    };
  });

  // output/Web.UIEvent.MouseEvent/foreign.js
  function clientX(e) {
    return e.clientX;
  }
  function clientY(e) {
    return e.clientY;
  }
  function button(e) {
    return e.button;
  }

  // output/Web.UIEvent.MouseEvent/index.js
  var fromEvent2 = /* @__PURE__ */ unsafeReadProtoTagged("MouseEvent");

  // output/FRP.Event.Mouse/index.js
  var map11 = /* @__PURE__ */ map(functorEffect);
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var wrap4 = /* @__PURE__ */ wrap();
  var insert8 = /* @__PURE__ */ insert2(ordInt);
  var $$delete7 = /* @__PURE__ */ $$delete2(ordInt);
  var withPosition = function(v) {
    return function(e) {
      return makeEvent(function(k) {
        return subscribe(e)(function(value12) {
          return function __do4() {
            var pos = read(v.position)();
            return k({
              value: value12,
              pos
            })();
          };
        });
      });
    };
  };
  var getMouse = function __do2() {
    var position2 = $$new(Nothing.value)();
    var buttons2 = $$new(empty3)();
    var target5 = map11(toEventTarget)(windowImpl)();
    var mouseMoveListener = eventListener(function(e) {
      return traverse_4(function(me) {
        return write(new Just({
          x: clientX(me),
          y: clientY(me)
        }))(position2);
      })(fromEvent2(e));
    })();
    var mouseDownListener = eventListener(function(e) {
      return traverse_4(function(me) {
        return modify(insert8(button(me)))(buttons2);
      })(fromEvent2(e));
    })();
    var mouseUpListener = eventListener(function(e) {
      return traverse_4(function(me) {
        return modify($$delete7(button(me)))(buttons2);
      })(fromEvent2(e));
    })();
    addEventListener(wrap4("mousemove"))(mouseMoveListener)(false)(target5)();
    addEventListener(wrap4("mousedown"))(mouseDownListener)(false)(target5)();
    addEventListener(wrap4("mouseup"))(mouseUpListener)(false)(target5)();
    var dispose = function __do4() {
      removeEventListener(wrap4("mousemove"))(mouseMoveListener)(false)(target5)();
      removeEventListener(wrap4("mousedown"))(mouseDownListener)(false)(target5)();
      return removeEventListener(wrap4("mouseup"))(mouseUpListener)(false)(target5)();
    };
    return {
      position: position2,
      buttons: buttons2,
      dispose
    };
  };
  var down2 = /* @__PURE__ */ makeEvent(function(k) {
    return function __do4() {
      var target5 = map11(toEventTarget)(windowImpl)();
      var mouseDownListener = eventListener(function(e) {
        return traverse_4(function(me) {
          return k(button(me));
        })(fromEvent2(e));
      })();
      addEventListener(wrap4("mousedown"))(mouseDownListener)(false)(target5)();
      return removeEventListener(wrap4("mousedown"))(mouseDownListener)(false)(target5);
    };
  });

  // output/Main/index.js
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var map12 = /* @__PURE__ */ map(functorEffect);
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordInt)(ordInt);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable(ordTuple2)(foldableArray);
  var $$for2 = /* @__PURE__ */ $$for(applicativeEffect)(traversableArray);
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var useMailboxed2 = /* @__PURE__ */ useMailboxed(ordTuple2);
  var lcmap3 = /* @__PURE__ */ lcmap(profunctorFn);
  var alt7 = /* @__PURE__ */ alt(altEvent);
  var pure15 = /* @__PURE__ */ pure(applicativeEvent);
  var lookup2 = /* @__PURE__ */ lookup(ordTuple2);
  var mapFlipped1 = /* @__PURE__ */ mapFlipped(functorFn);
  var fold4 = /* @__PURE__ */ fold2(eventIsEvent);
  var add4 = /* @__PURE__ */ add(/* @__PURE__ */ semiringTuple(semiringInt)(semiringInt));
  var map13 = /* @__PURE__ */ map(functorEvent);
  var compact2 = /* @__PURE__ */ compact(compactableEvent);
  var mapFlipped22 = /* @__PURE__ */ mapFlipped(functorEvent);
  var gate2 = /* @__PURE__ */ gate(eventIsEvent);
  var sub1 = /* @__PURE__ */ sub(/* @__PURE__ */ ringEvent(/* @__PURE__ */ ringRecord()(/* @__PURE__ */ ringRecordCons({
    reflectSymbol: function() {
      return "x";
    }
  })()(/* @__PURE__ */ ringRecordCons({
    reflectSymbol: function() {
      return "y";
    }
  })()(ringRecordNil)(ringInt))(ringInt))));
  var $$void8 = /* @__PURE__ */ $$void(functorEffect);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect)(foldableSet);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var pureAttr2 = /* @__PURE__ */ pureAttr(attrSvg_WidthString);
  var show3 = /* @__PURE__ */ show(showInt);
  var pureAttr1 = /* @__PURE__ */ pureAttr(attrSvg_HeightString);
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var mapFlipped32 = /* @__PURE__ */ mapFlipped(functorArray);
  var pureAttr22 = /* @__PURE__ */ pureAttr(attrPattern_IdString);
  var pureAttr3 = /* @__PURE__ */ pureAttr(attrPattern_WidthString);
  var pureAttr4 = /* @__PURE__ */ pureAttr(attrPattern_HeightString);
  var pureAttr5 = /* @__PURE__ */ pureAttr(attrPattern_ViewBoxString);
  var intercalate4 = /* @__PURE__ */ intercalate2(monoidString);
  var pureAttr6 = /* @__PURE__ */ pureAttr(attrImage_HrefString);
  var pureAttr7 = /* @__PURE__ */ pureAttr(attrImage_XString);
  var pureAttr8 = /* @__PURE__ */ pureAttr(attrImage_YString);
  var pureAttr9 = /* @__PURE__ */ pureAttr(attrImage_WidthString);
  var pureAttr10 = /* @__PURE__ */ pureAttr(attrImage_HeightString);
  var mapAttr2 = /* @__PURE__ */ mapAttr(functorEvent);
  var mapAttr1 = /* @__PURE__ */ mapAttr2(attrRect_XString);
  var mapAttr22 = /* @__PURE__ */ mapAttr2(attrRect_YString);
  var pureAttr11 = /* @__PURE__ */ pureAttr(attrRect_WidthString);
  var pureAttr12 = /* @__PURE__ */ pureAttr(attrRect_HeightString);
  var pureAttr13 = /* @__PURE__ */ pureAttr(attrRect_FillString);
  var mapAttr3 = /* @__PURE__ */ mapAttr2(attrRect_StrokeString);
  var pureAttr14 = /* @__PURE__ */ pureAttr(attrRect_StrokeWidthStrin);
  var pureAttr15 = /* @__PURE__ */ pureAttr(attrOnMousedownEffectUnit);
  var insert9 = /* @__PURE__ */ insert2(ordTuple2);
  var mapAttr4 = /* @__PURE__ */ mapAttr2(attrOnMouseupEffectUnit);
  var disj2 = /* @__PURE__ */ disj(/* @__PURE__ */ heytingAlgebraEvent(heytingAlgebraBoolean));
  var width8 = 3840;
  var pieces_wide = 16;
  var pieces_high = 9;
  var pieces = /* @__PURE__ */ function() {
    return apply(applyArray)(map(functorArray)(Tuple.create)(range(0)(pieces_wide - 1 | 0)))(range(0)(pieces_high - 1 | 0));
  }();
  var piece_width = /* @__PURE__ */ div2(width8)(pieces_wide);
  var height8 = 2160;
  var piece_height = /* @__PURE__ */ div2(height8)(pieces_high);
  var main = function __do3() {
    var selected2 = $$new(empty3)();
    var mouse = getMouse();
    var random_starts = map12(fromFoldable4)($$for2(pieces)(function(v) {
      return function __do4() {
        var x$prime = mapFlipped3(randomInt(0)(width8))(function(v1) {
          return v1 - (v.value0 * piece_width | 0) | 0;
        })();
        var y$prime = mapFlipped3(randomInt(0)(height8))(function(v1) {
          return v1 - (v.value1 * piece_height | 0) | 0;
        })();
        return new Tuple(new Tuple(v.value0, v.value1), new Tuple(x$prime, y$prime));
      };
    }))();
    return runInBody(bind3(useState(true))(function(v) {
      return bind3(useMailboxed2)(function(v1) {
        return bind3(function() {
          var $129 = lcmap3(function(v2) {
            return new Tuple(v2.value0, function(address) {
              return alt7(v2.value1(address))(pure15(fromMaybe(new Tuple(0, 0))(lookup2(address)(random_starts))));
            });
          });
          return function($130) {
            return useMailboxed2($129($130));
          };
        }())(function(v2) {
          var sink = function(event) {
            return switcherFlipped(event)(function(v3) {
              return blank;
            });
          };
          var receive_pos = mapFlipped1(v2.value1)(fold4(add4)(new Tuple(0, 0)));
          var receive_x = mapFlipped1(receive_pos)(map13(fst));
          var receive_y = mapFlipped1(receive_pos)(map13(snd));
          var mouse_position = compact2(mapFlipped22(withPosition(mouse)(animationFrame))(function(v3) {
            return v3.pos;
          }));
          var is_shift_up = compact2(mapFlipped22(up)(function(v3) {
            if (v3 === "ShiftLeft") {
              return new Just(false);
            }
            ;
            return Nothing.value;
          }));
          var is_shift_down = compact2(mapFlipped22(down)(function(v3) {
            if (v3 === "ShiftLeft") {
              return new Just(true);
            }
            ;
            return Nothing.value;
          }));
          var is_shift = alt7(pure15(false))(alt7(is_shift_down)(is_shift_up));
          var drag = gate2(v.value1)(sub1(mouse_position)(mouse_position));
          var has_been_dragging = alt7(mapFlipped22(v.value1)($$const(false)))(mapFlipped22(drag)($$const(true)));
          var move_selected = makeEvent(function(k) {
            return function __do4() {
              $$void8(subscribe(drag)(function(v3) {
                var move = new Tuple(v3.x, v3.y);
                return function __do5() {
                  var to_move = read(selected2)();
                  for_4(to_move)(function(address) {
                    return v2.value0({
                      address,
                      payload: move
                    });
                  })();
                  return k(move)();
                };
              }))();
              return pure8(unit);
            };
          });
          var clicked_outside = gate2(mapFlipped22(v.value1)(not2))(mapFlipped22(down2)($$const(true)));
          var unselect_all = makeEvent(function(v3) {
            return function __do4() {
              $$void8(subscribe(clicked_outside)(function(e) {
                if (e) {
                  return function __do5() {
                    var to_unselect = read(selected2)();
                    for_4(to_unselect)(function(address) {
                      return v1.value0({
                        address,
                        payload: false
                      });
                    })();
                    return write(empty3)(selected2)();
                  };
                }
                ;
                return pure8(unit);
              }))();
              return pure8(unit);
            };
          });
          return svg(alt7(pureAttr2(Width.value)(show3(width8 * 2 | 0)))(pureAttr1(Height.value)(show3(height8 * 2 | 0))))(append5([sink(move_selected), sink(unselect_all), defs_(mapFlipped32(pieces)(function(v3) {
            return pattern(alt7(pureAttr22(Id.value)("background-" + (show3(v3.value0) + ("-" + show3(v3.value1)))))(alt7(pureAttr3(Width.value)("100%"))(alt7(pureAttr4(Height.value)("100%"))(pureAttr5(ViewBox.value)(intercalate4(" ")(mapFlipped32([0, 0, piece_width, piece_height])(show3)))))))([image(alt7(pureAttr6(Href.value)("ship-1366926_crop_4k.png"))(alt7(pureAttr7(X.value)(show3((-v3.value0 | 0) * piece_width | 0)))(alt7(pureAttr8(Y.value)(show3((-v3.value1 | 0) * piece_height | 0)))(alt7(pureAttr9(Width.value)(show3(width8)))(pureAttr10(Height.value)(show3(height8)))))))([])]);
          }))])(mapFlipped32(pieces)(function(v3) {
            return rect(alt7(mapAttr1(X.value)(mapFlipped22(mapFlipped22(receive_x(new Tuple(v3.value0, v3.value1)))(function(v4) {
              return v4 + (v3.value0 * piece_width | 0) | 0;
            }))(show3)))(alt7(mapAttr22(Y.value)(mapFlipped22(mapFlipped22(receive_y(new Tuple(v3.value0, v3.value1)))(function(v4) {
              return v4 + (v3.value1 * piece_height | 0) | 0;
            }))(show3)))(alt7(pureAttr11(Width.value)(show3(piece_width)))(alt7(pureAttr12(Height.value)(show3(piece_height)))(alt7(pureAttr13(Fill.value)("url(#background-" + (show3(v3.value0) + ("-" + (show3(v3.value1) + ")")))))(alt7(mapAttr3(Stroke.value)(mapFlipped22(v1.value1(new Tuple(v3.value0, v3.value1)))(function(v4) {
              if (v4) {
                return "orange";
              }
              ;
              if (!v4) {
                return "none";
              }
              ;
              throw new Error("Failed pattern match at Main (line 175, column 58 - line 177, column 44): " + [v4.constructor.name]);
            })))(alt7(pureAttr14(StrokeWidth.value)("5"))(alt7(pureAttr15(OnMousedown.value)(function __do4() {
              v1.value0({
                address: new Tuple(v3.value0, v3.value1),
                payload: true
              })();
              modify_(insert9(new Tuple(v3.value0, v3.value1)))(selected2)();
              return v.value0(true)();
            }))(mapAttr4(OnMouseup.value)(mapFlipped22(disj2(has_been_dragging)(is_shift))(function(v4) {
              if (v4) {
                return v.value0(false);
              }
              ;
              if (!v4) {
                return function __do4() {
                  var to_unselect = read(selected2)();
                  for_4(to_unselect)(function(address) {
                    return v1.value0({
                      address,
                      payload: false
                    });
                  })();
                  v1.value0({
                    address: new Tuple(v3.value0, v3.value1),
                    payload: true
                  })();
                  write(singleton4(new Tuple(v3.value0, v3.value1)))(selected2)();
                  return v.value0(false)();
                };
              }
              ;
              throw new Error("Failed pattern match at Main (line 185, column 63 - line 193, column 55): " + [v4.constructor.name]);
            })))))))))))([]);
          })));
        });
      });
    }))();
  };

  // <stdin>
  main();
})();
