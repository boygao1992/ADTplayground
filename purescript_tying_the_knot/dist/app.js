// Generated by purs bundle 0.12.1
var PS = {};
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var Control_Category = PS["Control.Category"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ring = PS["Data.Ring"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
    "use strict";

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(
      /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
      function (c, i) {
        switch (c) {
          case "\"":
          case "\\":
            return "\\" + c;
          case "\x07": return "\\a";
          case "\b": return "\\b";
          case "\f": return "\\f";
          case "\n": return "\\n";
          case "\r": return "\\r";
          case "\t": return "\\t";
          case "\v": return "\\v";
        }
        var k = i + 1;
        var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty;
      }
    ) + "\"";
  };

  exports.cons = function (head) {
    return function (tail) {
      return [head].concat(tail);
    };
  };

  exports.join = function (separator) {
    return function (xs) {
      return xs.join(separator);
    };
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Data.Symbol"];      
  var SProxy = (function () {
      function SProxy() {

      };
      SProxy.value = new SProxy();
      return SProxy;
  })();
  var IsSymbol = function (reflectSymbol) {
      this.reflectSymbol = reflectSymbol;
  };
  var reflectSymbol = function (dict) {
      return dict.reflectSymbol;
  };
  exports["IsSymbol"] = IsSymbol;
  exports["reflectSymbol"] = reflectSymbol;
  exports["SProxy"] = SProxy;
})(PS["Data.Symbol"] = PS["Data.Symbol"] || {});
(function(exports) {
    "use strict";

  exports.unsafeGet = function (label) {
    return function (rec) {
      return rec[label];
    };
  };
})(PS["Record.Unsafe"] = PS["Record.Unsafe"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Record.Unsafe"];
  exports["unsafeGet"] = $foreign.unsafeGet;
})(PS["Record.Unsafe"] = PS["Record.Unsafe"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var RLProxy = (function () {
      function RLProxy() {

      };
      RLProxy.value = new RLProxy();
      return RLProxy;
  })();
  exports["RLProxy"] = RLProxy;
})(PS["Type.Data.RowList"] = PS["Type.Data.RowList"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Data.Show"];
  var Data_Symbol = PS["Data.Symbol"];
  var Record_Unsafe = PS["Record.Unsafe"];
  var Type_Data_RowList = PS["Type.Data.RowList"];                 
  var Show = function (show) {
      this.show = show;
  };
  var ShowRecordFields = function (showRecordFields) {
      this.showRecordFields = showRecordFields;
  };
  var showString = new Show($foreign.showStringImpl);
  var showRecordFieldsNil = new ShowRecordFields(function (v) {
      return function (v1) {
          return [  ];
      };
  });
  var showRecordFields = function (dict) {
      return dict.showRecordFields;
  };
  var showRecord = function (dictRowToList) {
      return function (dictShowRecordFields) {
          return new Show(function (record) {
              var v = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);
              if (v.length === 0) {
                  return "{}";
              };
              return $foreign.join(" ")([ "{", $foreign.join(", ")(v), "}" ]);
          });
      };
  }; 
  var show = function (dict) {
      return dict.show;
  };
  var showRecordFieldsCons = function (dictIsSymbol) {
      return function (dictShowRecordFields) {
          return function (dictShow) {
              return new ShowRecordFields(function (v) {
                  return function (record) {
                      var tail = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);
                      var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                      var focus = Record_Unsafe.unsafeGet(key)(record);
                      return $foreign.cons($foreign.join(": ")([ key, show(dictShow)(focus) ]))(tail);
                  };
              });
          };
      };
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["ShowRecordFields"] = ShowRecordFields;
  exports["showRecordFields"] = showRecordFields;
  exports["showString"] = showString;
  exports["showRecord"] = showRecord;
  exports["showRecordFieldsNil"] = showRecordFieldsNil;
  exports["showRecordFieldsCons"] = showRecordFieldsCons;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];                 
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];                 
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];                 
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Data.Function.Uncurried"];
  var Data_Unit = PS["Data.Unit"];                 
  var runFn1 = function (f) {
      return f;
  };
  exports["runFn1"] = runFn1;
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Effect"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Prelude = PS["Prelude"];                 
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applyEffect"] = applyEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS["Effect"] = PS["Effect"] || {});
(function(exports) {
    "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Effect.Console"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Effect = PS["Effect"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function(exports) {
    "use strict";

  exports.new = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports["modify'"] = function (f) {
    return function (ref) {
      return function () {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function(exports) {
  // Generated by purs version 0.12.1
  "use strict";
  var $foreign = PS["Effect.Ref"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Effect = PS["Effect"];
  var Prelude = PS["Prelude"];                 
  var modify = function (f) {
      return $foreign["modify'"](function (s) {
          var s$prime = f(s);
          return {
              state: s$prime,
              value: s$prime
          };
      });
  };
  var modify_ = function (f) {
      return function (s) {
          return Data_Functor["void"](Effect.functorEffect)(modify(f)(s));
      };
  };
  exports["modify"] = modify;
  exports["modify_"] = modify_;
  exports["new"] = $foreign["new"];
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function(exports) {exports._unRef = function (ref) {
    return ref.value
  }
})(PS["Main"] = PS["Main"] || {});
(function(exports) {
    "use strict";
  var $foreign = PS["Main"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Symbol = PS["Data.Symbol"];
  var Data_Unit = PS["Data.Unit"];
  var Effect = PS["Effect"];
  var Effect_Console = PS["Effect.Console"];
  var Effect_Ref = PS["Effect.Ref"];
  var Prelude = PS["Prelude"];                 
  var Post = function (x) {
      return x;
  };
  var User = function (x) {
      return x;
  };

  // | without class Lazy
  var A = function (x) {
      return x;
  };
  var B = function (x) {
      return x;
  };
  var OneToOne = function (oneToOne) {
      this.oneToOne = oneToOne;
  };
  var OneToMany = function (oneToMany) {
      this.oneToMany = oneToMany;
  };
  var ManyToMany = function (manyToMany) {
      this.manyToMany = manyToMany;
  };

  // HACK the returned value is not immutable!!!
  // it's ok as long as those values are contained locally without polluting the rest of the library or client's code base
  var unRef = Data_Function_Uncurried.runFn1($foreign._unRef);
  var step = function (l) {
      return l(Data_Unit.unit);
  };
  var showPost = new Data_Show.Show(function (v) {
      return "Post { " + (Data_Show.show(Data_Show.showString)(v.id) + " }");
  });
  var oneToOne = function (dict) {
      return dict.oneToOne;
  };
  var oneToMany = function (dict) {
      return dict.oneToMany;
  };
  var manyToMany = function (dict) {
      return dict.manyToMany;
  };

  // posts <- Ref.new []
  // author <- Ref.new Nothing
  // let wenbo = User { name : "wenbo", posts }
  //     post1 = Post { id : "001", author }
  //     wenbo2 = case wenbo of
  //       User r -> User r { name = "wenbo2"}
  // Ref.modify_ (const $ Just wenbo) author
  // Ref.modify_ (const $ [ post1 ]) posts
  // logShow $ case wenbo2 of
  //   User r -> unRef r.posts
  var main = function __do() {
      var v = Effect_Ref["new"]({
          a: {
              b: "wenbo"
          },
          c: "robot"
      })();
      var x = (function (v1) {
          return v1.a;
      })(unRef(v));
      Effect_Ref.modify_(function (v1) {
          return {
              a: {
                  b: "wenbo1"
              },
              c: v1.c
          };
      })(v)();
      Effect_Console.logShow(Data_Show.showRecord()(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "b";
      }))(Data_Show.showRecordFieldsNil)(Data_Show.showString)))(x)();
      return Data_Unit.unit;
  };
  var b0 = function (v) {
      return {
          a: step(a0)
      };
  };
  var a0 = function (v) {
      return {
          b: step(b0)
      };
  };
  exports["manyToMany"] = manyToMany;
  exports["oneToMany"] = oneToMany;
  exports["oneToOne"] = oneToOne;
  exports["User"] = User;
  exports["Post"] = Post;
  exports["A"] = A;
  exports["B"] = B;
  exports["step"] = step;
  exports["a0"] = a0;
  exports["b0"] = b0;
  exports["unRef"] = unRef;
  exports["OneToOne"] = OneToOne;
  exports["OneToMany"] = OneToMany;
  exports["ManyToMany"] = ManyToMany;
  exports["main"] = main;
  exports["showPost"] = showPost;
  exports["_unRef"] = $foreign._unRef;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();