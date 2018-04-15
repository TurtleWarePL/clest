# CLEST – Common Lisp Tests go East

This software is not meant as another Unit Testing Framework like Fiveam, Prove
or Fiasco. It is close in spirit to TestLink – test managament software for
Quality Assurance teams in various projects. Name is a non-translatable pun
which originates from English idiom "go south" – basically it means that tests:

* does not go south (because they go east!)
* it is not exactly the opposite direction (ditto!)

## Forest structure

CLEST instance maintains a collection of projects. Project is a central figure
in the tests architecture. It is a root node of the following (distinct) aspects
of the software maintanance:

* Test Suite tree

<!-- * Testing Plan / Build tree -->
<!-- * Requirements and documentation -->

Test suites divide project into functionalities which need to be tested. For a
web browser project that could be: the renderer, JS compiler, UX etc. Each test
suite may have other test (sub-)suites and test scenarios as its children.

Test scenario is a usage pattern and it may have zero or more test cases. For
instance scenario may be "add a bookmark" while test cases could be: add
bookmark to a website, add bookmark to a file, add bookmark to invalid object
(should signal error).

Software tends to grow and something being a small feature may become a
module. For such situation test scenarios may be upgradeed to test suites (and
in consequence their test cases become test scenarios).

<!-- Test plans are meant for test managament (as in human assignments). We could do -->
<!-- full regression testing before a new release or have a plan for testing specific -->
<!-- module which has been recently changed (or implemented). Each test plan may have -->
<!-- associated many builds (platforms or succeeding release candidates). -->

<!-- Requirements are software blueprints. This aspect embodies things like -->
<!-- specification, documentation, reference manual, technical notes etc. It may also -->
<!-- contain an issue tracker and other entities which doesn't belong to the first -->
<!-- two categories. -->

<!-- ### Technical note -->

<!-- Documentation as of now covers only Test Suite tree protocol. Testing plan, -->
<!-- build, requirement and documentation nodes are just opaque objects until we -->
<!-- implement them correctly (and define protocols for them). Testing plan should be -->
<!-- part of this software while requirements and documentation are something we need -->
<!-- to think about. -->

## Core protocol

To ensure reasonable extensibility software is aranged around protocols. We
provide a basic implementation which may be inherited from, but nothing prevents
a programmer to subclass directly protocol classes and implement methods for
them – other components should interoperate with them neverless. In this sense
CLEST design is stratified.

### Core protocol classes

    clest:project
    clest:test-suite
    clest:test-scenario
    clest:test-case

<!-- clest:testing-plan -->
<!-- clest:build -->
<!-- clest:requirement -->
<!-- clest:documentation -->

### Core protocol conditions

    clest:child-already-exists
    clest:child-doesnt-exist
    clest:invalid-designator

<!-- clest:invalid-parent-type -->

### Synopsis protocol

    clest:name object

Returns object's designator which is a string.

    clest:description (object clest:project)

Returns object's description. Its type is deliberely not specified but it should
be printable with princ in a human-readable manner (aesthetically). It is object
meant to be displayed by the software as a project's summary or its entry point.


### Test Suite tree protocol

PARENT type is intentionally not specified. If children are projects it may be a
CLIM applicatino frame, a database, a hash table etc. In that case this
parameter is meant to be specialized by the protocol implementer.

Children are designated by their names which are strings. Parent can't have two
children having the same name (names must be unique among sibling
nodes). Siblings are not necessarily of the same type (for instance a project
may have both test-suite and test-scenario as its direct children).

    clest:list-children parent

Returns a sequence of all parent direct children.

    clest:save-project parent (object clest:project)

Makes OBJECT part of the parent's collection. If parent already has a child of
the same name, a condition of type CLEST:CHILD-ALREADY-EXISTS is signalled.

    clest:load-child parent (name string)

Looks for a parent's child. If a project does not exist in parent's collection a
condition of type CLEST:CHILD-DOESNT-EXIST is signalled. Otherwise returns
requested node.

    clest:delete-child parent (name string)

Deletes a child from parent. If a project does not exist in parent's collection
a condition of type CLEST:CHILD-DOESNT-EXIST is signalled.

### Project protocol

Project must obey `synopsis` and `test suite tree` protocols. Additionally the
following functions must be defined:

    clest:make-project type &key name parent &allow-other-keys

Project constructor. TYPE must be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must obey the `project parent` protocol.

    clest:extensions (object clest:project)

Returns a sequence of defined extensions. Function is here for a forward
compatibility with extensions like issue tracker, test plans, reporting
capabilities etc.

### Test suite protocol

Test suite must obey `synopsis` and `test suite tree` protocols. Additionally
the following functions are defined:

    clest:make-test-suite type &key name parent

Test suite constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be either a project or another test
suite.

### Test scenario protocol

Test scenario must obey `synopsis` and `test suite tree` protocols. Additionally
the following functions are defined:

    clest:make-scenario type &key name parent

Test scenario constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be either a project or a test suite.

    clest:promote-to-test-suite type (object clest:test-scenario)

As applications grow some test scenarios may grow into test suites. This method
returns a test-suite object. All test-case children should be promoted to test
scenarios.

### Test case protocol

Test cases must obey `synopsis` protocol. Additionally the following functions
are defined:

    clest:make-test-case type &key name parent

Test case constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be a test scenario.

    clest:promote-to-test-scenario type (object clest:test-case)

As applications grow some test cases become scenarios. This method returns a
test-scenario object.
