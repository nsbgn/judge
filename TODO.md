---
title: TODO
---


Composing rules with multiple instantiations                        {#compose}
===============================================================================

At the moment, the sort of rules I want to use are not possible. They rely on 
the idea that either *at least one* of the rules applicable to a node must 
lead to a closed subtableau, or that *all* of them do.

The first issue is discussed in [#overlapping]. I propose to deal with the 
second issue by adding a `compose` key to the system. This key determines what 
to do with the different instantiations of a rule. The possible values will 
be:

- `nondeterministic`: Default. Different instantiations simply produce 
  different rules. Since the rules will overlap, this means that we may have 
  to try out different paths until we find one that leads to a closed tableau. 

- `greedy`: As it is now. When there are multiple instantiations of a rule, we 
  will assume that the first applicable one is the one we need.

- `disjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, each under its own branch. In practice, this will have 
  the effect that the rule will require closure under *each* instantiation --- 
  this as opposed to the `nondeterministic` value, where it requires closure 
  under *at least one* instantiation.

- `conjunctive`: The productions of each (applicable) instantiation on a node 
  are taken together, and appended to the branch one after the other. 

Of these, `disjunctive` and `nondeterministic` are the ones that are vital for 
my use case, and the other two are natural duals (but they could be left out 
because YAGNI).



Overlapping rules                                               {#overlapping}
===============================================================================

At the moment, the sort of rules I want to use are not possible. They rely on 
the idea that either *at least one* of the rules applicable to a node must 
lead to a closed subtableau, or that *all* of them do.

The second issue is discussed in [#compose]. For the first issue, we must make 
sure that overlapping rules are handled properly. We can treat the *order* of 
rule application as irrelevant (in terms of computability, not complexity), 
but when we have choice *on the same node*, we cannot take this greedy 
approach. 

Instead, when two or more rules are applicable to the same non-empty set of 
unprocessed formulas on the branch, we should try them out, until we find one 
that leads to closure. (If we could, we would try them all and only keep the 
shortest one, but that is likely prohibitively expensive.)


