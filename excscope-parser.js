
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

// abbreviations for syntactic categories
var cats = {
  NP: {
    con:'Leaf',
    targs:['NP']
  },
  S: {
    con:'Leaf',
    targs:['S']
  },
  MNP: {
    con:'M',
    targs:[{con:'Leaf', targs:['NP']}]
  },
  MS: {
    con:'M',
    targs:[{con:'Leaf', targs:['S']}]
  },
  KNP: {
    con:'K',
    targs:[
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'M', targs:[{con:'Leaf', targs:['S']}]},
      {con:'Leaf', targs:['NP']}
    ]
  }
};

// dictionary of lexical entries for words in the fragment
var wordMeanings = {

  Ann : {
    phn: 'Ann',
    sem: ann,
    syn: cats.NP
  },

  Bob : {
    phn: 'Bob',
    sem: bob,
    syn: cats.NP
  },
  
  Cal : {
    phn: 'Cal',
    sem: cal,
    syn: cats.NP
  },

  student : {
    phn: 'student',
    sem: function(x){return function(world){return findw(world, x).student;};},
    syn: {con:'B', targs:[cats.NP, cats.S]}
  },
  
  nice : {
    phn: 'nice',
    sem: function(x){return function(world){return findw(world, x).nice;};},
    syn: {con:'B', targs:[cats.NP, cats.S]}
  },

  tall : {
    phn: 'tall',
    sem: function(x){return function(world){return findw(world, x).tall;};},
    syn: {con:'B', targs:[cats.NP, cats.S]}
  },

  likes : { // nice people like nice people; mean people like mean people
    phn: 'likes',
    sem: function(x) {
      return function(y) {
        return function(world) {
          return findw(world, x).nice === findw(world, y).nice;
        };
      };
    },
    syn: {con:'F', targs:[{con:'B', targs:[cats.NP, cats.S]}, cats.NP]}
  },

  somebody : {
    phn: 'somebody',
    sem: function(s){return domain.map(function(x){return [x, s.concat([x])];});},
    syn: cats.MNP
  },

  neg : {
    phn: 'neg',
    sem: function(m) {
      return function(s) {
        var ms = m(s); // [(prop1, s1), (prop2, s2), ... ]
        return [[function(world){return !truth(ms, world);}, s]];
      };
    },
    syn: {con:'F', targs:[cats.MS, cats.MS]}
  },

  every : {
    phn: 'every',
    sem: function(c) {
      return function(k) {
        return function(s) {
          return [ [
            function(world) {
              return domain.every(function(x) {
                return c(x)(s).every(function(o) {
                  return !o[0](world) || truth(k(x)(o[1].concat([x])), world);
                });
              });
            }, s
          ] ];
        };
      };
    },
    syn: {con: 'K', targs: [cats.KNP, cats.MS, cats.NP]}
  },

  everybody : {
    phn: 'everybody',
    sem: function(k) {
      return function(s) {
        return [ [
          function(world) {
            return domain.every(function(x){return truth(k(x)(s), world);});
          }, s
        ] ];
      };
    },
    syn: cats.KNP
  },

};


/*****************
 * Interpretation
 *****************/

// takes in a sentence; returns a distribution over worlds
var literalListener = function(utterance) {
  Enumerate(function() {
    // get the meanings of all successful parses of the utterance
    var ms = interpret(utterance).map(function(deriv){return deriv.sem;});
    // randomly choose one of those meanings
    var disamb = Math.floor(sample(uniformERP, [0, ms.length]));
    var m = ms[disamb];
    // build a random world (with that meaning in mind, so to speak)
    var world = worldPrior(domain, m);
    // ignore worlds in which the meaning is false
    factor(m(world) ? 0 : -Infinity);
    return world;
  }, 100);
};

// takes in a domain of individuals and a proposition;
// build entities with random properties one at a time, attempting to stay
// true to the meaning;
// once all domain individuals have been assigned properties in the world,
// returns the model
var worldPrior = function(dom, prop, worldSoFar, prevFactor) {
  worldSoFar = worldSoFar || [];
  prevFactor = prevFactor || 0;
  if (dom.length !== 0) { // if the model is still missing individuals, ...
    var newObj = makeObj(dom[0]); // randomly attach properties to entity 
    var newWorld = worldSoFar.concat([newObj]); // add new entity to the model
    var newFactor = prop(newWorld) ? 0: -100; // see if new model is  
                                                     // consistent with meaning
    factor(newFactor - prevFactor); // downweight the world if it isn't
    // continute
    return worldPrior(dom.slice(1), prop, newWorld, newFactor);
  } else { // once we've built a complete model, return it
    factor(-prevFactor);
    return worldSoFar;
  }
};

// takes in name; returns an entity with random properties;
var makeObj = function(name) {
  return {
    id: name,
    blond: flip(0.5),
    nice: flip(0.5),
    tall: flip(0.5)
  };
};


/*********************
 * Grammar, Semantics
 *********************/

// takes in an utterance; parses it into a list of lexical items;
// returns the final denotations of any successful sentence-sized derivations
// built out of those items
var interpret = function(utterance) {
  var words = parse(utterance);
  // ignore undefined words
  var defined_words = words.filter(function(wrd){return wrd.sem;});
  // go ahead and nondeterministically combine any adjacent lexical items
  // with the right types
  var initial_edges = edgify(defined_words);
  // iterate combining until nothing left to combine
  var all_edges = closeUnder(findEdges, initial_edges);
  // keep only those edges that span all of the words, and form a sentence
  var full_sentences = all_edges.filter(function(e) {
    var full = e.stop - e.start === defined_words.length; 
    var sentence = e.cat.con === 'M' && e.cat.targs[0].targs[0] === 'S' ||
                   e.cat.con === 'Leaf' && e.cat.targs[0] === 'S';
    return full && sentence;
  });
  // return the meanings of those edges
  return full_sentences;
};

// takes in a string; returns a list of lexical items
var parse = function(utterance) {
  return utterance.split(' ').map(function(wrd) {
    return lookupWord(wrd);
  });
};

// retrieve a word's lexical entry from the fragment dictionary
var lookupWord = function(word) {
  var m = wordMeanings[word];
  return m || {phn: word, syn: '', sem: undefined};
};

// convert list of lexical entries to list of trivial edges
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

// close a set under an operation
var closeUnder = function(op, oldl) {
  // iteratively apply an operation 'op' to a set 'oldl' until nothing new
  // generated
  var newl = union(oldl, op(oldl)); 
  return (newl.length === oldl.length) ? oldl : closeUnder(op, newl);
};

// attempt to combine two edges, using fixed suite of binary combinators
var combine = function(lsyn, rsyn) {
  var combs = [];
  var cmbs; // potential inner combinations (for 2-step combinators)
  var rcmbs; // potential recursive combinations (for 2-step combinators)
  var lcmbs; // potential lowered combinations
  
  // Forward Application
  if (lsyn.con === 'F' && cat_equal(lsyn.targs[1], rsyn)) {
    combs = combs.concat([{
      rule: 'id',
      sem: function(L){return function(R){return L(R);};},
      cat: lsyn.targs[0]
    }]);
  }
  // Backward Application
  if (rsyn.con === 'B' && cat_equal(rsyn.targs[0], lsyn)) {
    combs = combs.concat([{
      rule: '(flip id)',
      sem: function(L){return function(R){return R(L);};},
      cat: rsyn.targs[1]
    }]);
  }

  // Right Lift
  if (lsyn.con === 'K') {
    cmbs = combine(lsyn.targs[2], rsyn);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' L (lift R))',
          sem: function(L) {
            return function(R) {
              return function(k) {
                return L(function(y){return k(cmb.sem(y)(R));});
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
    cmbs = combine(lsyn, rsyn.targs[2]);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' (lift L) R)',
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

  // Right MLift
  if (rsyn.con === 'M') {
    cmbs = combine(lsyn, rsyn.targs[0]);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' (lift L) (mlift R))',
          sem: function(L) {
            return function(R) {
              return function(k) {
                return bind(R)(function(y){return k(cmb.sem(L)(y));});
              };
            };
          },
          cat: {con:'K', targs:[cats.MS, cats.MS, cmb.cat]}
        };
      });
      combs = combs.concat(rcmbs);
    }
  }
  // Left MLift
  if (lsyn.con === 'M') {
    cmbs = combine(lsyn.targs[0], rsyn);
    if (cmbs.length !== 0) {
      rcmbs = cmbs.map(function(cmb) {
        return {
          rule: '(liftM2 ' + cmb.rule + ' (mlift L) (lift R))',
          sem: function(L) {
            return function(R) {
              return function(k) {
                return bind(L)(function(x){return k(cmb.sem(x)(R));});
              };
            };
          },
          cat: {con:'K', targs:[cats.MS, cats.MS, cmb.cat]}
        };
      });
      combs = combs.concat(rcmbs);
    }
  }

  // Right Unit
  if (lsyn.con === 'F' && lsyn.targs[1].con === 'M' &&
      cat_equal(lsyn.targs[1].targs[0], rsyn)) {
    combs = combs.concat([{
      rule: '(. unit)',
      sem: function(L) {
        return function(R) {
          return L(function(s){return [[R,s]];});
        };
      },
      cat: lsyn.targs[0]
    }]);
  }
  // Left Unit
  if (rsyn.con === 'B' && rsyn.targs[1].con === 'M' &&
      cat_equal(lsyn, rsyn.targs[1].targs[0])) {
    combs = combs.concat([{
      rule: '(flip ($) . unit)',
      sem: function(L) {
        return function(R) {
          return R(function(s){return [[L,s]];});
        };
      },
      cat: rsyn.targs[1]
    }]);
  }

  // Lower 
  lcmbs = combs.map(function(cmb) {
    var c = cmb.cat;
    if (c.con === 'K' && cat_equal(c.targs[1], c.targs[2])) {
      return {
        rule: '(lower $ ' + cmb.rule + ')',
        sem: function(L) {
          return function(R) {
            return cmb.sem(L)(R)(function(x){return x;});
          };
        },
        cat: c.targs[0]
      };
    }
  });
  // MLower
  mlcmbs = combs.map(function(cmb) {
    var c = cmb.cat;
    if (c.con === 'K' && c.targs[1].con == 'M' &&
        cat_equal(c.targs[1].targs[0], c.targs[2])) {
      return {
        rule: '(mlower $ ' + cmb.rule + ')',
        sem: function(L) {
          return function(R) {
            return cmb.sem(L)(R)(function(x){return function(s){return [[x,s]];};});
          };
        },
        cat: c.targs[0]
      };
    }
  });

  // ditch any Lower operations that aren't defined
  lcmbs = lcmbs.filter(function(x){return x;});
  combs = combs.concat(lcmbs);

  // ditch any MLower combinations that aren't defined
  mlcmbs = mlcmbs.filter(function(x){return x;});
  combs = combs.concat(mlcmbs);

  return combs;
};


/*******************************
 * Auxiliary Semantic Functions
 *******************************/

// returns the entity named @name, with all its properties, as instantiated
// in world @world
var findw = function(world, name) {
  return world.filter(function(x){return x.id === name;})[0];
};

// state monad bind operator
var bind = function(m) {
  return function(k) {
    return function(s) {
      var ms = m(s); // [[x1,s1], [x2,s2], ...]
      return flatten(ms.map(function(e){return k(e[0])(e[1]);}));
    };
  };
};

// dynamic truth: has at least one true output
var truth = function(outputs, world) {
  return outputs.some(function(o){return o[0](world);});
};


/****************************
 * Auxiliary Logic Functions
 ****************************/

// quick and dirty generalized union
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
      // return c1.targs.reduce(function(acc, c1, i) {
      //   return acc && cat_equal(c1, c2.targs[i]);
      // }, true);
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
      // return e1.daughters.reduce(function(acc, d1, i) {
      //   return acc && edge_equal(d1, e2.daughters[i]);
      // }, true);
    }
  }
  // if either preconditions of recursion over daughters fails, not equal
  return false;
};

// concatenate two lists and remove duplicates
var union = function(l1, l2) {
  var flat = l1.concat(l2);
  return flat.filter(function(value, index, self) {
    return indexOf(flat, value, edge_equal) === index;
  });
};

// return index of (last) occurrence of @v in @l, or -1 if @v not in @l;
// values are compared by @comp
var indexOf = function(l, v, comp) {
  return l.reduce(function(a,b,i) {
    return comp(b,v) ? i : a;
  }, -1);
};

