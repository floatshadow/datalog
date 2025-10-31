///|
typealias @util.Name

///|
pub struct NaiveInterpreter {
  mut counter : Int
  tracer : @util.SubTracer
}

///|
pub fn NaiveInterpreter::new(tracer : @util.SubTracer) -> NaiveInterpreter {
  NaiveInterpreter::{ counter: 0, tracer }
}

///|
fn NaiveInterpreter::new_temp(self : NaiveInterpreter) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

///|
fn NaiveInterpreter::new_named_temp(
  self : NaiveInterpreter,
  name : Name,
) -> Name {
  let counter = self.counter
  self.counter += 1
  Name::{ ..name, slot: counter }
}

///|
fn NaiveInterpreter::join_iterator(
  self : NaiveInterpreter,
  subgoals : Array[Literal],
  facts : Database,
) -> MultiProductIterator {
  let multi_iter = subgoals.map(fn(subgoal) {
    let relation = subgoal.predicate
    match facts.get(relation) {
      None => ArrayIterator::from([])
      Some(facts) => ArrayIterator::from(facts.iter().to_array())
    }
  })
  MultiProductIterator::{ multi_iter, }
}

///|
fn NaiveInterpreter::immediate_operator(
  self : NaiveInterpreter,
  rules : HashMap[String, Array[Clause]],
  arity : ArityMap,
  facts : Database,
) -> (ArityMap, Database) raise Failure {
  let mut new_facts = facts
  let mut new_arity = arity
  for rules_for_pred in rules {
    let (pred, rules) = rules_for_pred
    for rule in rules {
      self.tracer.log("Processing rule \{rule}")
      let { head, body } = rule
      let mut subst = Some(HashMap::new())
      new_arity = new_arity.check(body)
      for subgoal in body {
        let relation = subgoal.predicate
        let lits = match new_facts.get(relation) {
          None => HashSet::new()
          Some(facts) => facts
        }
        for lit in lits {
          // Try to unify the subgoal with the literal
          subst = subst_mapM(subst, fn(subst) {
            subst.unify_literal(subgoal, lit)
          })
        }
      }
      let iterator = self.join_iterator(body, new_facts)
      while true {
        guard iterator.next() is Some(product) else { break }
        self.tracer.log("  Considering product \{product}\n")
        let mut subst = Some(HashMap::new())
        for i = 0; i < product.length(); {
          let subgoal = body[i]
          let lit = product[i]
          self.tracer.log("    Unifying subgoal \{subgoal} with fact \{lit}")
          subst = subst_mapM(subst, fn(s) { s.unify_literal(subgoal, lit) })
          continue i + 1
        }
        match subst {
          None => ()
          Some(subst) => {
            self.tracer.log("  Found substitution \{Show::to_string(subst)}")
            let head = head.apply_subst(subst)
            guard head.is_fact() else {
              fail("Deduced head \{head} is not a fact")
            }
            self.tracer.log("  Deduced fact \{head}")
            new_facts = new_facts.insert_multi(pred, head)
          }
        }
      }
      // special case
      if head.is_fact() {
        self.tracer.log("  Deduced fact \{head}")
        new_facts = new_facts.insert_multi(pred, head)
      }
    }
  }
  (new_arity, new_facts)
}

///|
/// Evaluate a list of statements (facts/rules/queries) using the naive fixpoint algorithm.
pub fn NaiveInterpreter::evaluate(
  self : NaiveInterpreter,
  stmts : List[@syntax.Statement],
) -> Array[Literal] raise Failure {
  let mut arity = ArityMap::new()
  let mut rules = HashMap::new()
  let query = []
  // collect rules and queries
  for stmt in stmts {
    match stmt {
      AssertS({ inner: clause, .. }) => {
        let head = Literal::from(clause.head.inner)
        let head_predicate = head.predicate
        let body = match clause.body {
          None => []
          Some(body) =>
            body.to_array().map(fn(lit) { Literal::from(lit.inner) })
        }
        rules = rules.add_multi(head_predicate, Clause::{ head, body })
      }
      QueryS({ inner: lit, .. }) => query.push(Literal::from(lit))
    }
  }
  // naive evaluation
  let mut facts = Database::new()
  let mut changed = true
  let mut iteration = 0
  while changed {
    self.tracer.log("Naive iteration \{iteration}")
    self.tracer.log("Current facts: \{Show::to_string(facts)}")
    iteration += 1
    let old_facts = facts
    let (new_arity, new_facts) = self.immediate_operator(rules, arity, facts)
    self.tracer.log("New facts: \{Show::to_string(new_facts)}")
    arity = new_arity
    facts = new_facts
    if old_facts == new_facts {
      changed = false
    }
  }

  // answer queries
  let results = []
  for q in query {
    println("query \{q}")
    let lits = match facts.get(q.predicate) {
      None => HashSet::new()
      Some(facts) => facts
    }
    for lit in lits {
      let subst = HashMap::new()
      match subst.unify_literal(q, lit) {
        None => ()
        Some(s) => results.push(lit.apply_subst(s))
      }
    }
  }
  results
}
