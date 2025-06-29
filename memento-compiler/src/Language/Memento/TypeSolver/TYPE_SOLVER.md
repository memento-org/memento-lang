# Memento Type Solver - Current Implementation

## Overview

This document describes the constraint-based type solver for the Memento programming language. The solver handles advanced type system features including polymorphism, union/intersection types, subtyping, and exhaustive pattern matching.

## Type System

### Core Types

```haskell
data Type
  = TTop | TBottom          -- Top and bottom types
  | TNumber | TBool | TString | TNever | TUnknown  -- Primitive types
  | TLiteral Literal        -- Literal types (42, true, "hello")
  | TVar TypeVar           -- Fresh type variables (unification variables)
  | TConstructor T.Text    -- Named constructors (distinct ground types)
  | TFunction Type Type    -- Function types
  | TUnion (Set Type)      -- Union types (A | B)
  | TIntersection (Set Type)  -- Intersection types (A & B)
  -- Polymorphism support
  | TGeneric T.Text           -- Generic type parameters (T, U, V)
  | TApplication Type [Type]  -- Type applications (List<T>, Map<K,V>)
```

### Polymorphism Support

- **Type Schemes**: `TypeScheme [vars] type` represents `forall vars. type`
- **Generalization**: Convert monomorphic types to polymorphic type schemes
- **Instantiation**: Create fresh type variables when using polymorphic values
- **Variance Analysis**: Automatic detection of covariant/contravariant type parameters

### Constraint Language

The solver works with subtype constraints:

```haskell
data Constraint = Subtype Type Type  -- t1 <: t2
```

## Algorithm Overview

The type solver follows a multi-step constraint solving algorithm:

```
1. Constraint Decomposition
2. Contradiction Detection
3. Boundary Calculation
4. Type Unification
5. Branch Splitting (if needed)
```

The algorithm repeats steps 1-4 until either:

- **Success**: All constraints solved with consistent substitution
- **Contradiction**: Inconsistent constraints detected
- **Ambiguity**: Multiple solutions exist, handled by branching

## Detailed Algorithm Steps

### Step 1: Constraint Decomposition

Decomposes complex constraints into simpler forms by applying structural rules:

#### Union Type Decomposition

```
or(A, B, C) <: T  →  A <: T, B <: T, C <: T
```

#### Intersection Type Decomposition

```
S <: and(A, B, C)  →  S <: A, S <: B, S <: C
```

#### Function Type Decomposition (Contravariant in arguments)

```
(A1 -> R1) <: (A2 -> R2)  →  A2 <: A1, R1 <: R2
```

#### Type Application Decomposition (with variance awareness)

```
F<A1, B1> <: F<A2, B2>  →  constraints based on parameter variance
```

### Step 2: Contradiction Detection

Identifies constraints that are immediately solvable or contradictory:

#### Checkable Constraints

- **Concrete types**: `number <: bool` (can check immediately)
- **Reflexivity**: `x <: x` (always true)
- **Generic reflexivity**: `T <: T` (always true for same generic)
- **Function vs non-function**: `(A -> B) <: number` (always false)

#### Generic Type Handling

- **Same generics**: `T <: T` → no contradiction
- **Different generics**: `T <: U` → potential contradiction
- **Generic vs concrete**: `T <: number` → no immediate contradiction

### Step 3: Boundary Calculation

For each type variable `x`, collects all lower and upper bounds:

```
{lower1, lower2, ...} <: x <: {upper1, upper2, ...}
```

#### Bidirectional Constraint Preservation

- **Critical Fix**: Preserves all constraint information without removing constraints
- Handles generic type bounds: `x <: T`, `T <: x`
- Maintains relationships between multiple variables

#### Bound Extraction

- **Lower bounds**: From constraints `T <: x`
- **Upper bounds**: From constraints `x <: T`
- **Generic bounds**: From constraints involving `TGeneric` types

### Step 4: Type Unification

Attempts to find a unique type for variables with convergent bounds:

#### Sound Unification Logic

```haskell
unifyBounds :: Bounds -> Maybe Type
unifyBounds (Bounds lowers uppers) =
  case findCommonTypes lowers uppers of
    [] -> Nothing        -- No common type
    [t] -> Just t       -- Exactly one common type
    _ -> Nothing        -- Multiple common types = ambiguous
```

#### Critical Soundness Fix

- **Old (unsound)**: `([t], []) -> Just t` - could assign any supertype
- **New (sound)**: Only unify when type appears in **both** bounds
- **Rationale**: Ensures assigned type satisfies all constraints

#### Unification Exclusions

- Type variables are not unified (remain as variables)
- Generic types are not unified (represent parameters, not unknowns)

### Step 5: Branch Splitting

When no progress can be made, splits ambiguous constraints:

#### Union Type Branching

```
x <: A | B  →  Branch 1: x <: A
              Branch 2: x <: B
```

#### Intersection Type Branching

```
A & B <: x  →  Branch 1: A <: x
              Branch 2: B <: x
```

#### Generic Type Branching

- Supports branching with generic types
- Handles `T <: A | B` and `A & B <: T`

## Polymorphism Integration

### Type Parameter Binding

1. **Declaration**: `val identity<T> : (x : T) => T`
2. **Generalization**: Create `TypeScheme ["T"] (TFunction (TGeneric "T") (TGeneric "T"))`
3. **Instantiation**: When used, replace `T` with fresh type variables

### Constraint Generation with Generics

- Generic types `TGeneric "T"` participate in subtyping
- Automatic variance analysis determines parameter relationships
- Type applications `List<T>` decompose according to variance

### Variance Analysis

```haskell
data Variance = Covariant | Contravariant | Invariant

-- Examples:
data Box<T> : (value : T) => Box        -- T is covariant
data Processor<T> : (f : (T) => number) => Processor  -- T is contravariant
data Cell<T> : (get : () => T, set : (T) => ()) => Cell  -- T is invariant
```

## Pattern Matching Integration

### Exhaustivity Constraints

The type solver integrates with pattern matching through exhaustivity constraints:

```haskell
-- For switch expressions, generate constraint:
⋀ max-bounds <: ⋁ min-bounds

-- Where:
-- max-bounds = union of all possible constructor types
-- min-bounds = union of all covered pattern types
```

### Pattern Type Bounds

- **Max-bound**: What the pattern declares it matches (`(x : Option) => ...`)
- **Min-bound**: What the pattern actually covers (`Some(_)` covers `Some<T>`)
- **Exhaustivity**: Ensures all possible cases are covered

## Error Handling and Position Reporting

### Position-Aware Error Messages

- Constraints track source positions through AST metadata
- Error messages include file location and code range
- Enhanced debugging for polymorphic type errors

### Common Error Patterns

- **Missing pattern cases**: Exhaustivity constraint fails
- **Type mismatch**: Contradiction in constraint solving
- **Ambiguous types**: Multiple valid solutions exist
- **Infinite types**: Circular type constraints (detected during unification)

## Critical Soundness Properties

### 1. Unification Soundness

- **Property**: Unification only occurs when type appears in both bounds
- **Guarantee**: Assigned types satisfy all subtyping constraints
- **Prevents**: Unsound type assignments that violate program semantics

### 2. Bidirectional Constraint Preservation

- **Property**: `x <: y` generates bounds for both `x` and `y`
- **Guarantee**: All constraint relationships are preserved through solving
- **Prevents**: Lost constraint information leading to incorrect types

### 3. Generic Type Consistency

- **Property**: Generic types are handled uniformly throughout solver
- **Guarantee**: Polymorphic types maintain parametricity
- **Prevents**: Generic type variables being unified incorrectly

## Implementation Architecture

### Module Structure

```
TypeSolver/
├── Types.hs          -- Core type definitions, polymorphism, variance
├── Solver.hs         -- Main constraint solving algorithm
├── ConstraintGen.hs  -- Constraint generation from AST
├── Subtype.hs        -- Subtyping algorithm
├── Normalize.hs      -- Type normalization (unions/intersections)
└── Demo.hs          -- Testing and examples
```

### Key Data Structures

```haskell
-- Constraint solving state
data InferContext = InferContext
  { icVarCounter :: Int                           -- Fresh variable counter
  , icConstraints :: ConstraintSet                -- Active constraints
  , icTypeEnv :: Map T.Text TypeScheme           -- Polymorphic type environment
  , icConstructors :: Set T.Text                 -- Known constructors
  , icVariances :: TypeConstructorVariances     -- Variance information
  }

-- Constraint solving results
data SolveResult
  = Success Substitution      -- Solution found
  | Contradiction            -- No solution exists
  | Ambiguous [ConstraintSet] -- Multiple solutions (branching needed)
```

## Future Extensions

### Planned Improvements

1. **Full Variance-Aware Decomposition**: Use variance information in type application decomposition
2. **Higher-Rank Polymorphism**: Support for rank-N types
3. **Dependent Types**: Limited dependent typing for array bounds, etc.
4. **Effect Types**: Integration with effect system for tracking side effects

### Research Directions

1. **Constraint Optimization**: More efficient constraint solving algorithms
2. **Incremental Solving**: Reuse solved constraints across compilation units
3. **Error Recovery**: Better error messages with suggested fixes
4. **Type Inference Debugging**: Visual constraint solving traces

## References

This implementation is based on:

- Pierce's "Types and Programming Languages" (TAPL)
- Dolan & Mycroft's "Polymorphism, Subtyping, and Type Inference in MLsub"
- Gaster & Jones's "A Polymorphic Type System for Extensible Records and Variants"
- Modern constraint-based type inference research

The algorithm maintains soundness while supporting advanced features like parametric polymorphism, union/intersection types, and exhaustive pattern matching.
