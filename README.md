GENERIC-COMPARABILITY
===

This is an implementation of CDR-8. [![Build Status](https://travis-ci.org/pnathan/generic-comparability.svg)](https://travis-ci.org/pnathan/generic-comparability)

CDR-8 provides an interface for the EQUALS function, which is defined
as a general equality predicate, as well as a set of ordering
(COMPARE) functions for comparison. The semantics are described in the
[CDR-8 standard](http://cdr.eurolisp.org/document/8/cleqcmp.html).

```
 EQUALS a b &rest keys &key recursive &allow-other-keys → result
```

```
COMPARE a b &rest keys &key recursive &allow-other-keys → result

COMPARE returns >, <, =, /=


LT a b &rest keys &key recursive &allow-other-keys → result
LTE a b &rest keys &key recursive &allow-other-keys → result
GT a b &rest keys &key recursive &allow-other-keys → result
GTE a b &rest keys &key recursive &allow-other-keys → result

The *T(E?) family returns T, NIL, or raises the INCOMPARABLE-OBJECT condition.

```



Contributing
---
Please issue a pull request.

Versioning
---
Semantic Versioning

License
---

LLGPL
