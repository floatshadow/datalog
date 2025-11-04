///|
typealias @util.Name

///|
pub struct Interpreter {
  mut counter : Int
  tracer : @util.SubTracer
}

///|
pub fn Interpreter::new(tracer : @util.SubTracer) -> Interpreter {
  Interpreter::{ counter: 0, tracer }
}

///|
fn Interpreter::new_temp(self : Interpreter) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

///|
fn Interpreter::new_named_temp(self : Interpreter, name : Name) -> Name {
  let counter = self.counter
  self.counter += 1
  Name::{ ..name, slot: counter }
}

///|
fn Interpreter::query(
  self : Interpreter,
  query : Literal,
  facts : Database,
) -> Unit raise Failure {
  println("query \{query}")
  let lits = match facts.get(query.predicate) {
    None => HashSet::new()
    Some(facts) => facts
  }
  let query_result = []
  for lit in lits {
    let subst = HashMap::new()
    match subst.unify_literal(query, lit) {
      None => ()
      Some(s) => query_result.push(lit.apply_subst(s))
    }
  }
  query_result.sort()
  for result in query_result {
    println("\{Show::to_string(result)}")
  }
}

///|
fn Interpreter::join_iterator(
  self : Interpreter,
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
fn Interpreter::immediate_operator(
  self : Interpreter,
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
      new_arity = new_arity.check(body)
      let iterator = self.join_iterator(body, new_facts)
      while true {
        guard iterator.next() is Some(product) else { break }
        let mut subst = Some(HashMap::new())
        // Try to unify the subgoal with the literal
        for i = 0; i < product.length(); {
          let subgoal = body[i]
          let lit = product[i]
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
pub fn Interpreter::naive_evaluate(
  self : Interpreter,
  stmts : List[@syntax.Statement],
) -> Unit raise Failure {
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
  for q in query {
    self.query(q, facts)
  }
}

///|
fn Interpreter::delta_join_iterator(
  self : Interpreter,
  subgoals : Array[Literal],
  facts~ : Database,
  delta_facts~ : Database,
  delta_state : Int,
) -> MultiProductIterator {
  let multi_iter = subgoals.mapi(fn(i, subgoal) {
    let relation = subgoal.predicate
    let lits = if ((delta_state >> i) & 1) == 1 {
      match delta_facts.get(relation) {
        None => HashSet::new()
        Some(facts) => facts
      }
    } else {
      match facts.get(relation) {
        None => HashSet::new()
        Some(facts) => facts
      }
    }
    ArrayIterator::from(lits.iter().to_array())
  })
  MultiProductIterator::{ multi_iter, }
}

///|
fn Interpreter::delta_immediate_operator(
  self : Interpreter,
  rules : HashMap[String, Array[Clause]],
  arity : ArityMap,
  facts~ : Database,
  delta_facts~ : Database,
) -> (Bool, ArityMap, Database) raise Failure {
  let mut new_delta_facts = Database::new()
  let mut new_arity = arity
  let mut changed = false
  for rules_for_pred in rules {
    let (pred, rules) = rules_for_pred
    for rule in rules {
      self.tracer.log("Processing rule \{rule}")
      let { head, body } = rule
      new_arity = new_arity.check(body)
      // P^(i) = P^(i-2) U ΔP^(i-1)
      let old_head_facts = match facts.get(pred) {
        None => HashSet::new()
        Some(facts) => facts
      }
      let old_head_delta_facts = match delta_facts.get(pred) {
        None => HashSet::new()
        Some(facts) => facts
      }
      let old_head_facts = old_head_facts.union(old_head_delta_facts)
      // enumerate all possible delta combinations
      let num_subgoals = body.length()
      let max_delta_state = 1 << num_subgoals
      for delta_state = 1
          delta_state < max_delta_state
          delta_state = delta_state + 1 {
        let iterator = self.delta_join_iterator(
          body,
          facts~,
          delta_facts~,
          delta_state,
        )
        while true {
          guard iterator.next() is Some(product) else { break }
          let mut subst = Some(HashMap::new())
          for i = 0; i < product.length(); {
            let subgoal = body[i]
            let lit = product[i]
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
              if not(old_head_facts.contains(head)) {
                new_delta_facts = new_delta_facts.insert_multi(pred, head)
                changed = true
              }
            }
          }
        }
      }
      // special case
      if head.is_fact() {
        self.tracer.log("  Deduced fact \{head}")
        if not(old_head_facts.contains(head)) {
          new_delta_facts = new_delta_facts.insert_multi(pred, head)
          changed = true
        }
      }
    }
  }
  (changed, new_arity, new_delta_facts)
}

///|
pub fn Interpreter::semi_naive_evaluate(
  self : Interpreter,
  stmts : List[@syntax.Statement],
) -> Unit raise Failure {
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
  // semi-naive evaluation
  let mut facts = Database::new()
  let mut delta_facts = Database::new()
  let mut iteration = 0
  while true {
    self.tracer.log("Semi-naive iteration \{iteration}")
    iteration += 1
    let (changed, new_arity, new_delta_facts) = self.delta_immediate_operator(
      rules,
      arity,
      facts~,
      delta_facts~,
    )
    self.tracer.log("New delta facts: \{Show::to_string(new_delta_facts)}")
    arity = new_arity
    facts = facts.merge(delta_facts)
    delta_facts = new_delta_facts
    if not(changed) {
      break
    }
  }

  // answer queries
  for q in query {
    self.query(q, facts)
  }
}
