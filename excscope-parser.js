
/*******************************
 * Auxiliary Semantic Functions
 *******************************/

// returns a world's instance of name, with all its properties in that world
var findw = function(world, name) {
  return world.filter(function(x){return x.id === name;})[0];
};

// State.List.Reader unit operator
var unit = function(phi) {
  return function(s) {
    return function(w) {
      return [[phi,s]];
    };
  };
};

// State.List.Reader bind operator
var bind = function(m) {
  // \k s w -> concat [k x s' w | (x,s') <- m s w]
  return function(k) {
    return function(s) {
      return function(w) {
        return flatten(m(s)(w).map(function(o){return k(o[0])(o[1])(w);}));
      };
    };
  };
};

// lift some intensional (w -> a) function through the
// Reader, List, State, and Cont monads
var wlift = function(phi) {
  return function(k) {
    return function(s) {
      return function(w) {
        return k(phi(w))(s)(w);
      };
    };
  };
};

// returns the constant function from worlds to phi
var constant = function(phi) {
  return function(w) { return phi; };
};

// dynamic truth: has at least one true output
var truth = function(outputs) {
  return outputs.some(function(o){return o[0];});
};


/****************************
 * Auxiliary Logic Functions
 ****************************/

// simple generlaized union
var flatten = function(things) {
  return things.reduce(function(thing1, thing2) {
    return thing1.concat(thing2);
  }, []);
};

// check whether two categories are equal
var cat_equal = function(c1, c2) {
  // some preconditions for two edges being equal
  var b = c1.con === c2.con &&
          c1.targs.length === c2.targs.length;
  if (b) { // if preconditions met, ...
    if (c1.con === 'Leaf') { // base case
      // leaves equal if their concrete types equal
      return c1.targs[0] === c2.targs[0];
    } else {
      // recursively check whether each of c1's type arguments is equal to the
      // corresponding type argument of c2
      return c1.targs.every(function(cat, i) {
        return cat_equal(cat, c2.targs[i]);
      });
    }
  }
  // if either preconditions or recursion over targs fails, not equal
  return false;
};

// check whether two edges are equal
var edge_equal = function(e1, e2) {
  // some preconditions for two edges being equal
  var b = e1.start === e2.start &&
          e1.stop === e2.stop &&
          e1.comb === e2.comb &&
          e1.daughters.length === e2.daughters.length;
  if (b) { // if preconditions met, ...
    if (e1.daughters.length === 0) { // base case
      // edges equal if they have no daughters
      return true;
    } else {
      // recursively check whether each of e1's daughters is equal to the
      // corresponding daughter of e2
      return e1.daughters.every(function(daughter, i) {
        return edge_equal(daughter, e2.daughters[i]);
      });
    }
  }
  // if either preconditions of recursion over daughters fails, not equal
  return false;
};

// concatenate two lists and remove duplicates
var union = function(l1, l2) {
  var flat = l1.concat(l2);
  return flat.filter(function(value, index) {
    return indexOfBy(flat, value, edge_equal) === index;
  });
};

// return index of (last) occurrence of v in l, or -1 if v not in l;
// values compared by supplied method, or === as default
var indexOfBy = function(l, v, comp) {
  comp = comp || function(x,y){return x === y;};
  return l.reduce(function(a,b,i){return comp(b,v) ? i : a;}, -1);
};

// iteratively apply an operation 'op' to a set 'oldl' until it doesn't
// generate anything new
var closeUnder = function(op, oldl) {
  var newl = union(oldl, op(oldl)); 
  return (newl.length === oldl.length) ? oldl : closeUnder(op, newl);
};


/*************************
 * Domain, Model, Language
 *************************/

// entities individuated for convenience by their names (as strings)
var ann = 'Ann';
var bob = 'Bob';
var cal = 'Cal';

// domain consists of three people
var domain = [ann, bob, cal];

// demo world
var w = [
  {id:ann, student:true, nice:false, tall:false},
  {id:bob, student:true, nice:true, tall:false},
  {id:cal, student:true, nice:false, tall:false}
];

// abbreviations for syntactic categories (really semantic types)
var cats = {
  NP: {
    con:'Leaf',
    targs:['NP']
  },
  S: {
    con:'Leaf',
    targs:['S']
  },
  VP: {
    con:'B',
    targs:[{con:'Leaf', targs:['NP']}, {con:'Leaf', targs:['S']}]
  },
  MNP: {
    con:'M',
    targs:[{con:'Leaf', targs:['NP']}]
  },
  MS: {
    con:'M',
    targs:[{con:'Leaf', targs:['S']}]
  },
  MVP: {
    con:'M',
    targs:[
      {con:'B', targs:[{con:'Leaf', targs:['NP']}, {con:'Leaf', targs:['S']}]}
    ]
  },
  KNP: {
    con:'K',
    targs:[
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'Leaf', targs:['NP']}
    ]
  },
  KS: {
    con:'K',
    targs:[
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'Leaf', targs:['S']}
    ]
  },
  KVP: {
    con:'K',
    targs:[
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'B', targs:[{con:'Leaf', targs:['NP']}, {con:'Leaf', targs:['S']}]}
    ]
  }
};

// dictionary of lexical entries for words in the fragment
var wordMeanings = {

  Ann : {
    phn: 'Ann',
    sem: wlift(function(w){return ann;}),
    syn: cats.KNP
  },

  Bob : {
    phn: 'Bob',
    sem: wlift(function(w){return bob;}),
    syn: cats.KNP
  },
  
  Cal : {
    phn: 'Cal',
    sem: wlift(function(w){return cal;}),
    syn: cats.KNP
  },

  pro0 : {
    phn: 'pro0',
    sem: function(k) {
      var _pro0 = function(s) {
        return function(w) {
          return [[s[s.length - 1], s]];
        };
      };
      return bind(_pro0)(k);
    },
    syn: cats.KNP
  },

  student : {
    phn: 'student',
    sem: wlift(function(w){return function(x){return findw(w,x).student;};}),
    syn: cats.KVP
  },
  
  nice : {
    phn: 'nice',
    sem: wlift(function(w){return function(x){return findw(w,x).nice;};}),
    syn: cats.KVP
  },

  tall : {
    phn: 'tall',
    sem: wlift(function(w){return function(x){return findw(w,x).tall;};}),
    syn: cats.KVP
  },

  likes : { // nice people like nice people; mean people like mean people
    phn: 'likes',
    sem: wlift(function(w) {
      return function(x) {
        return function(y) {
          return findw(w,x).nice === findw(w,y).nice;
        };
      };
    }),
    syn: {
      con:'K',
      targs:[
        cats.MS,
        cats.MS,
        {con:'F', targs:[{con:'B', targs:[cats.NP, cats.S]}, cats.NP]}
      ]
    }
  },

  some : {
    phn: 'some',
    sem: function(c) {
      var _somec = function(s) {
        return function(w) {
          var outs = domain.map(function(x) {
            return c(x)(s)(w).filter(function(o){return o[0];})
                             .map(function(o){return [x,o[1].concat([x])];});
          });
          return flatten(outs);
        };
      };
      return bind(_somec);
    },
    syn: {con:'K', targs:[cats.KNP, cats.MS, cats.NP]}
  },

  somebody : {
    phn: 'somebody',
    sem: function(k) {
      var _somebody = function(s) {
        return function(w) {
          return domain.map(function(x){return [x, s.concat([x])];});
        };
      };
      return bind(_somebody)(k);
    },
    syn: cats.KNP
  },

  neg : {
    phn: 'neg',
    sem: function(m) {
      return function(s) {
        return function(w) {
          return [[!truth(m(s)(w)), s]];
        };
      };
    },
    syn: {con:'F', targs:[cats.MS, cats.MS]}
  },

  every : {
    phn: 'every',
    sem: function(c) {
      return function(k) {
        return function(s) {
          return function(w) {
            return [ [
              domain.every(function(x) {
                return c(x)(s)(w).every(function(o) {
                  return !o[0] || truth(k(x)(o[1].concat([x]))(w));
                });
              }), s
            ] ];
          };
        };
      };
    },
    syn: {con: 'K', targs: [cats.KNP, cats.MS, cats.NP]}
  },

  everybody : {
    phn: 'everybody',
    sem: function(k) {
      return function(s) {
        return function(w) {
          return [[domain.every(function(x){return truth(k(x)(s)(w));}), s]];
        };
      };
    },
    syn: cats.KNP
  },

};


/*****************
 * Interpretation
 *****************/

// takes in a sentence and discourse context;
// searches for worlds that make the sentence true in the context
var literalListener = function(utterance, context) {
  context = context || [];
  Enumerate(function() {
    // get the meanings of all successful parses of the utterance
    var ms = interpret(utterance).map(function(deriv){return deriv.sem;});
    // randomly choose one of those meanings
    var disamb = Math.floor(sample(uniformERP, [0, ms.length]));
    var m = ms[disamb];
    // build a random world (with that meaning in mind, so to speak)
    var world = worldPrior(domain, m, context);
    // ignore worlds in which the meaning is false
    factor(truth(m(context)(world)) ? 0 : -Infinity);
    return world;
  }, 100);
};

// takes in a domain of individuals and a meaning (and possibly a context);
// builds a model by adding entities with random properties one at a time,
// attempting to stay true to the meaning (in the context);
// returns the model once all domain individuals have been assigned properties
var worldPrior = function(dom, meaning, context, worldSoFar, prevFactor) {
  context = context || [];
  worldSoFar = worldSoFar || [];
  prevFactor = prevFactor || 0;
  if (dom.length !== 0) { // if the model is still missing individuals, ...
    var newObj = makeObj(dom[0]); // randomly attach properties to entity 
    var newWorld = worldSoFar.concat([newObj]); // add new entity to the model
    // see if new model is consistent with meaning;
    // if not, downweight this execution path
    var newFactor = truth(meaning(context)(newWorld)) ? 0: -100;
    factor(newFactor - prevFactor);
    // keep on worldbuilding
    return worldPrior(dom.slice(1), meaning, newWorld, newFactor);
  } else { // once we've built a complete model, return it
    factor(-prevFactor);
    return worldSoFar;
  }
};

// build an entity with random properties
var makeObj = function(name) {
  return {
    id: name,
    student: flip(0.5),
    nice: flip(0.5),
    tall: flip(0.5)
  };
};


/*********************
 * Grammar, Semantics
 *********************/

// parse an utterance into a list of lexical items;
// return the final denotations of any successful derivations with cateogry
// asCategory, which defaults to cats.MS
var interpret = function(utterance, asCategory) {
  asCategory = asCategory || cats.MS;
  var words = parse(utterance);
  // ignore undefined words
  var defined_words = words.filter(function(wrd){return wrd.sem;});
  // go ahead and nondeterministically combine any adjacent lexical items
  // with the right types
  var initial_edges = edgify(defined_words);
  // iterate combining until nothing left to combine
  var all_edges = closeUnder(findEdges, initial_edges);
  // keep only those edges that span all of the words, and form the right
  // cateogry
  return all_edges.filter(function(e) {
    var full = e.stop - e.start === defined_words.length; 
    var cat = cat_equal(e.cat, asCategory);
    return full && cat;
  });
};

// convert a string into a list of lexical items
var parse = function(utterance) {
  return utterance.split(' ').map(lookupWord);
};

// retrieve a word's lexical entry from the fragment dictionary
var lookupWord = function(word) {
  var m = wordMeanings[word];
  return m || {phn: word, syn: '', sem: undefined};
};

// convert a list of lexical entries to a list of trivial edges
var edgify = function(words) {
  return words.map(function(wrd, i) {
    return {
      phrase: wrd.phn,
      cat: wrd.syn,
      sem: wrd.sem,
      start: i,
      stop: i+1,
      comb: '',
      daughters: []
    };
  });
};

// single step search for new combinable edges
var findEdges = function(es) {
  // build list of adjacent pairs
  var edge_pairs = flatten(
    es.map(function(e1) {
      return es.map(function(e2) {
        return e1.stop === e2.start ? [e1, e2] : [];
      }).filter(function(pair){
        return pair.length > 0;
      });
    })
  );
  // generate new edges for all combinable pairs
  var new_edges = flatten(
    edge_pairs.map(function(edge_pair) {
      var e1 = edge_pair[0];
      var e2 = edge_pair[1];
      var cmbs = combine(e1.cat, e2.cat);
      return cmbs.map(function(cmb) {
        return {
          phrase: e1.phrase + ' ' + e2.phrase,
          cat: cmb.cat,
          sem: cmb.sem(e1.sem)(e2.sem),
          start: e1.start,
          stop: e2.stop,
          comb: cmb.rule,
          daughters: [e1, e2]
        };
      });
    })
  );
  // return the new edges
  return new_edges;
};

// attempt to combine two edges, using fixed suite of combinators
var combine = function(lsyn, rsyn) {
  var combs = [];
  var cmbs; // potential inner combinations (for recursive combinators)
  var rcmbs; // inner combinations lifted to higher towers
  var lcmbs; // potential lowered combinations
  var llcmbs; // potential reset combinations (lifted after lowered)
  
  // Forward Application
  if (lsyn.con === 'F' && cat_equal(lsyn.targs[1], rsyn)) {
    combs = combs.concat([{
      rule: 'id',
      sem: function(L){return function(R){return L(R);};},
      cat: lsyn.targs[0]
    }]);
  }
  // Backward Application
  if (rsyn.con === 'B' && cat_equal(lsyn, rsyn.targs[0])) {
    combs = combs.concat([{
      rule: '(flip id)',
      sem: function(L){return function(R){return R(L);};},
      cat: rsyn.targs[1]
    }]);
  }

  // Right Lift
  if (lsyn.con === 'K') {
    // lift the right arg one level; then try to combine left and right
    cmbs = combine(lsyn.targs[2], rsyn);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' L (lift R))',
          // \l r -> do x <- l
          //            y <- return r
          //            return (cmb.sem x y)
          sem: function(L) { 
            return function(R) {
              return function(k) {
                return L(function(x){return k(cmb.sem(x)(R));});
              };
            };
          },
          cat: {con:'K', targs:[lsyn.targs[0], lsyn.targs[1], cmb.cat]}
        };
      });
      combs = combs.concat(rcmbs);
    }
  }
  // Left Lift
  if (rsyn.con === 'K') {
    // lift the left arg one level; then try to combine left and right
    cmbs = combine(lsyn, rsyn.targs[2]);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' (lift L) R)',
          // \l r -> do x <- return l
          //            y <- r
          //            return (cmb.sem x y)
          sem: function(L) {
            return function(R) {
              return function(k) {
                return R(function(y){return k(cmb.sem(L)(y));});
              };
            };
          },
          cat: {con:'K', targs:[rsyn.targs[0], rsyn.targs[1], cmb.cat]}
        };
      });
      combs = combs.concat(rcmbs);
    }
  }

  // Right Unit
  if (lsyn.con === 'F' && lsyn.targs[1].con === 'M' &&
      cat_equal(lsyn.targs[1].targs[0], rsyn)) {
    // inject the right arg into the monad; then try to combine left and right
    combs = combs.concat([{
      rule: '(. unit)',
      // \l r -> l (unit r)
      sem: function(L) {
        return function(R) {
          return L(unit(R));
        };
      },
      cat: lsyn.targs[0]
    }]);
  }
  // Left Unit
  if (rsyn.con === 'B' && rsyn.targs[0].con === 'M' &&
      cat_equal(lsyn, rsyn.targs[0].targs[0])) {
    // inject the left arg into the monad; then try to combine left and right
    combs = combs.concat([{
      rule: '(flip id . unit)',
      // \l r -> r (unit l)
      sem: function(L) {
        return function(R) {
          return R(unit(L));
        };
      },
      cat: rsyn.targs[1]
    }]);
  }

  var whatevs = combs.map(function(cmb) {
    var bs = findBinds(cmb.cat);
    return bs.map(function(b) {
      return {
        rule: '(' + b.rule + ' ' + cmb.rule + ')',
        sem: function(L) {
          return function(R) {
            return b.sem(cmb.sem(L)(R));
          };
        },
        cat: b.cat
      };
    });
  });
  combs = combs.concat(flatten(whatevs));

  // return the combinations
  return combs;
};

var findBinds = function(c) {
  var bs = [];
  if (c.con === 'M') {
    bs = bs.concat([{
      rule: 'bind',
      sem: bind,
      cat: {con:'K', targs:[cats.MS, cats.MS, c.targs[0]]}
    }]);
  }
  if (c.con === 'K') {
    bcmbs = findBinds(c.targs[2]);
    if (bcmbs.length !== 0) {
      rbcmbs = bcmbs.map(function(bcmb) {
        return {
          // \m -> do x <- m
          //          return (bcmb.rule x)
          rule: 'liftM $ ' + bcmb.rule,
          sem: function(m) {
            return function(k) {
              return m(function(y){return k(bcmb.sem(y));});
            };
          },
          cat: {con:'K', targs:[c.targs[0], c.targs[1], bcmb.cat]}
        };
      });
      bs = bs.concat(rbcmbs);
    }
  }
  return bs;
};

var findLowers = function(c) {
  var ls = [];
  if (c.con === 'K') {
    if (c.targs[1].con == 'M' && cat_equal(c.targs[1].targs[0], c.targs[2])) {
      ls = ls.concat([{
        rule: 'mlower',
        sem: unit,
        cat: c.targs[0]
      }]);
    }
    if (c.targs[2].con === 'K') {
      lcmbs = findLowers(c.targs[2]);
      if (lcmbs !== 0) {
        rlcmbs = lcmbs.map(function(lcmb) {
          return {
            rule: 'm' + lcmb.rule,
            sem: function(m){return m(lcmb.sem);},
            cat: ''
          };
        });
        ls = ls.concat(rclmbs);
      }
    }
  }
  return ls;
};
