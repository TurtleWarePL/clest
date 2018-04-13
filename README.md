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
* Testing Plan / Build tree
* Requirements and documentation

Test suites divide project into functionalities which need to be tested. For a
web browser project that could be: the renderer, JS compiler, UX etc. Each test
suite may have other test (sub-)suites and test cases as its children. Test case
is always a leaf node. Test case may be either manual or automated test.

Test plans are meant for test managament (as in human assignments). We could do
full regression testing before a new release or have a plan for testing specific
module which has been recently changed (or implemented). Each test plan may have
associated many builds (platforms or succeeding release candidates).

Requirements are software blueprints. This aspect embodies things like
specification, documentation, reference manual, technical notes etc. It may also
contain an issue tracker and other entities which doesn't belong to the first
two categories.

### Technical note

Documentation as of now covers only Test Suite tree protocol. Testing plan,
build, requirement and documentation nodes are just opaque objects until we
implement them correctly (and define protocols for them). Testing plan should be
part of this software while requirements and documentation are something we need
to think about.

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
    clest:testing-plan
    clest:build
    clest:requirement
    clest:documentation

### Core protocol conditions

    clest:child-already-exists
    clest:child-doesnt-exist

### Project parent protocol

PARENT type is intentionally not specified. It may be a CLIM application frame,
a database, a list etc. This parameter is meant to be specialized by the
protocol implementer. Projects are designated by their names which must be
unique in a scope of the parent.

    clest:list-projects parent

Returns a sequence of all projects which are parent direct children.

    clest:save-project parent (object clest:project)

Makes OBJECT part of the parent's collection. If parent already has a project
having the same name, a condition of type CLEST:CHILD-ALREADY-EXISTS is
signalled.

    clest:load-project parent (designator string)

Looks for a child in the parent's collection. Designator is project's name which
is a STRING. If a project does not exist in parent's collection a condition of
type CLEST:CHILD-DOESNT-EXIST is signalled. Otherwise returns requested project.

    clest:delete-project parent (designator string)

Deletes a project from the parent's collection. Designator is project's name
which is a string. If a project does not exist in parent's collection a
condition of type CLEST:CHILD-DOESNT-EXIST is signalled.

### Test suite parent protocol

PARENT of each test-suite must be a project or another test-suite. That means
that both PROJECT and TEST-SUITE must obey this protocol as parents.

    clest:list-test-suites parent
    clest:save-test-suite parent (object clest:test-suite)
    clest:load-test-suite parent (designator string)
    clest:delete-test-suite (designator string)

### Test case parent protocol

PARENT of each test-case must be a project or a test-suite. That means that both
PROJECT and TEST-SUITE must obey this protocol as parents.

    clest:list-test-cases parent
    clest:save-test-case parent (object clest:test-case)
    clest:load-test-case parent (designator string)
    clest:delete-test-case (designator string)

### Project protocol

    clest:list-test-suites (parent project)

Returns a sequence of test suites which are parent direct children.
