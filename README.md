# CLEST – Common Lisp Tests go East

This software is not meant as an another Unit Testing Framework like Fiveam,
Prove or Fiasco. It is close in spirit to TestLink – test managament software
for Quality Assurance teams. Name is a pun originating from English idiom "go
south" – basically it means that in this software tests:

* do not go south (because they go east!)
* it is not exactly the opposite direction (ditto!)

**Important note** This software is still a work in progress and it is not meant
to be used yet. When a minimal version is finished a proper announcement will be
published (and this note will be removed).

## Contributing

We are still working on the core protocol, documentation, system internals and
figuring out other concepts important for the software development. We do not
accept contributions for a time being.

## Forest structure

CLEST instance maintains a collection of projects. Project is a central figure
in the tests architecture. It is a root node of the following (distinct) aspects
of the software maintanance:

### Test suites

Test suites divide project into functionalities which need to be tested. For a
web browser that could be: the renderer, JS compiler, UX etc. Each test suite
may have other test (sub-)suites and test scenarios as its children.

Test scenario describes a usage pattern and it may have zero or more test
cases. Example scenario could be "add a bookmark" and cases could be: "add
bookmark to a website", "add bookmark to a file", "add bookmark to an invalid
object (should signal error)".

### Testing plans and software builds

Test plans are meant for test execution managament. We could do full regression
testing before a new release or have a plan for testing specific module which
has been recently modified (or implemented).

Each test plan may have many builds associated. Builds may have different
commits or may be built from the same commit but for different platforms etc.

Requirements are software blueprints. This aspect embodies things like
specification, documentation, reference manual, technical notes etc. It may also
contain an issue tracker and other entities which doesn't belong to the first
two categories.

## Core protocol

To ensure reasonable extensibility software is aranged around protocols. We
provide a basic implementation which may be inherited from, but nothing prevents
a programmer to subclass directly protocol classes and implement methods for
them – other components should interoperate with them neverless. In this sense
CLEST design is stratified.

### Classes

    CLEST:PROJECT
    CLEST:TEST-SUITE
    CLEST:TEST-SCENARIO
    CLEST:TEST-CASE

<!-- CLEST:TESTING-PLAN -->
<!-- CLEST:BUILD -->
<!-- CLEST:REQUIREMENT -->
<!-- CLEST:DOCUMENTATION -->

### Conditions

    CLEST:CLEST-ERROR (ERROR)
    CLEST:CHILD-ALREADY-EXISTS (CLEST-ERROR)
    CLEST:CHILD-DOESNT-EXIST (CLEST-ERROR)
    CLEST:INVALID-DESIGNATOR (CLEST-ERROR)
    CLEST:INVALID-PARENT-TYPE (CLEST-ERROR)

### Synopsis

Synopsis protocol is defined for general entity description capabilities. It
defines two functions for purpose of listing them and providing their detail in
the UI.

    CLEST:NAME OBJECT

Returns object's designator which is a string.

    CLEST:DESCRIPTION OBJECT

Returns object's description. Its type is deliberely not specified but it should
be printable with princ in a human-readable manner (aesthetically).


### Test Suite tree protocol

Test suites are arranged in a tree. Each project is a tree root whose children
are either test suites or test scenarios. Test suite may be a parent of another
test suites and test scenarios. Test scenario may be a parent of test cases.

In the test suite tree protocol PARENT type is intentionally not specified. If
children are projects it may be a CLIM applicatino frame, a database, a hash
table etc. In that case this parameter is meant to be specialized by the
protocol implementer.

Children are designated by their names which are strings. Parent can't have two
children having the same name (names must be unique among sibling
nodes). Siblings are not necessarily of the same type (for instance a project
may have both test-suite and test-scenario as its direct descendents).

    CLEST:LIST-CHILDREN PARENT

Returns a sequence of parent direct children.

    CLEST:SAVE-CHILD PARENT OBJECT

Makes OBJECT part of the parent's collection. If parent already has a child of
the same name a condition of type CLEST:CHILD-ALREADY-EXISTS is signalled. If
PARENT can't be object's parent a condition of type CLEST:INVALID-PARENT-TYPE is
signalled.

    CLEST:LOAD-CHILD PARENT (NAME STRING)

Looks for a parent's child. If a project does not exist in parent's collection a
condition of type CLEST:CHILD-DOESNT-EXIST is signalled. Otherwise returns
requested node.

    CLEST:DELETE-CHILD PARENT (NAME STRING)

Deletes a child from parent. If a project does not exist in parent's collection
a condition of type CLEST:CHILD-DOESNT-EXIST is signalled.

### Project protocol

Project must obey `synopsis` and `test suite tree` protocols. Additionally the
following functions must be defined:

    CLEST:MAKE-PROJECT TYPE &KEY NAME PARENT &ALLOW-OTHER-KEYS

Project constructor. TYPE must be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must obey the `project parent` protocol.

    CLEST:EXTENSIONS (OBJECT CLEST:PROJECT)

Returns a sequence of defined extensions. Function is here for a forward
compatibility with extensions like issue tracker, test plans, reporting
capabilities etc.

### Test suite protocol

Test suite must obey `synopsis` and `test suite tree` protocols. Additionally
the following functions are defined:

    CLEST:MAKE-TEST-SUITE TYPE &KEY NAME PARENT

Test suite constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be either a project or another test
suite.

### Test scenario protocol

Test scenario must obey `synopsis` and `test suite tree` protocols. Additionally
the following functions are defined:

    CLEST:MAKE-SCENARIO TYPE &KEY NAME PARENT

Test scenario constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be either a project or a test suite.

### Test case protocol

Test cases must obey `synopsis` protocol. Additionally the following functions
are defined:

    CLEST:MAKE-TEST-CASE TYPE &KEY NAME PARENT

Test case constructor. TYPE should be specialized on a symbol with EQL
specializer. NAME is a string, PARENT must be a test scenario.
