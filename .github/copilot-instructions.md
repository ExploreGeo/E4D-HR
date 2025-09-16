# E4D-HR Copilot Instructions

## Project Overview

E4D-HR is a 3D geophysical modeling and inversion tool for direct current resistivity (DCR) and induced polarization (IP) surveys, forked from PNNL's E4D for mineral exploration applications. The codebase is primarily Fortran 90 with MPI parallelization, PETSc linear algebra, and TetGen/Triangle for mesh generation.

## Architecture & Core Components

### Master-Slave MPI Pattern
- **Master process** (`rank 0`): Orchestrates workflow, reads input, manages mesh, coordinates slaves
- **Slave processes** (`rank > 0`): Handle distributed forward modeling and Jacobian assembly
- Communication via `MPI_COMM_WORLD` and specialized sub-communicators (`SCOMM`, `LCOMM`)
- Entry point: `E4D.F90` → `distribute()` → `go_slave()` or master workflow

### Key Modules (src/)
- `VARS.F90`: Global variables, MPI ranks, PETSc contexts
- `MASTER.F90`: Master process control, mesh management, I/O
- `SLAVE.F90`: Slave process routines, distributed computation
- `DISTRIBUTOR.F90`: MPI processor allocation and communication setup
- `FORWARD.F90`: Forward modeling finite element solver
- `INVERT.F90`: Inversion algorithms with regularization
- `JACOBIAN.F90`: Sensitivity matrix computation
- `READ_INP.F90`: Input file parsing for all modes

### Run Modes
- `IP1`/`DCR1`: Mesh generation using TetGen with `.cfg` configuration
- `IP2`/`DCR2`: Forward modeling with survey files (`.srv`)
- `IP3`/`DCR3`: Inversion with data fitting and regularization
- Mode determined by `e4d.inp` first line

## Build System

### Dependencies & Environment
- **Required**: Intel oneAPI compilers (`ifx`, `icx`, `mpiifort`), PETSc 3.21.4, Intel MKL
- **Setup**: `source /opt/intel/oneapi/setvars.sh` before building
- **Environment check**: Verify `MKLROOT` is set

### Build Process
```bash
# Automatic installation (recommended)
python3 install.py

# Manual compilation in src/
make FC="mpiifort -fc=ifx" FFLAGS="-O2 -r8 -heap-arrays" PETSC_DIR=path/to/petsc
```

### Key Build Flags
- `-r8`: Double precision reals
- `-heap-arrays`: Avoid stack overflow for large arrays
- `-D resmode`: Enable resistivity mode compilation
- `PETSC_FC_INCLUDES`: PETSc header paths

## File Patterns & Conventions

### Input Files
- `e4d.inp`: Mode specification (required name, first line = run mode)
- `*.cfg`: Mesh generation configuration with zones, electrodes, constraints
- `*.srv`: Survey data with electrode positions and measurements
- `*.inv`: Inversion parameters for DCR/IP components
- `*.out`: Output options and visualization settings

### Output Files
- `*.1.node/.ele/.face/.edge/.neigh`: TetGen mesh components
- `*.trn`: Coordinate translations applied during meshing
- `*.sig`: Element conductivity/resistivity values
- `sigma.#`/`sigmai.#`: Inversion results per iteration
- `e4d.log`: Detailed execution log with timing and convergence

### Mesh Generation Workflow
1. Parse `.cfg` with zones, electrodes, refinement points
2. Call TetGen via `tetgen_path` executable
3. Apply coordinate translations stored in `.trn`
4. Generate `.sig` file with zone-based conductivity assignments

## Critical Development Patterns

### Error Handling
- Use `call nreport(error_code)` for standardized error messages
- Check MPI operations with `ierr` parameter
- PETSc operations end with `CHKERRQ(ierr)` or similar

### Memory Management
- Fortran allocatable arrays preferred over fixed arrays
- PETSc vectors/matrices managed via contexts (creation/destruction)
- Clean MPI communicators on exit with `call dist_abort`

### Parallel Computing
- All mesh and data distributed across slave processes
- Forward solve uses PETSc KSP iterative solvers
- Jacobian assembly distributed by measurement rows
- Collective operations for global reductions

### Precision & Numerics
- All reals are double precision (`real*8`)
- Complex conductivity: real + imaginary components
- Regularization via smoothness constraints and reference models

## Integration Points

### External Dependencies
- **TetGen**: Mesh generation, called via system command
- **Triangle**: 2D surface triangulation for topography
- **PETSc**: Linear algebra, solver contexts, parallel vectors/matrices
- **MPI**: Process communication and synchronization

### File I/O Conventions
- Fortran unformatted for binary mesh data
- ASCII for configuration and survey files
- VTK format for visualization output (optional)

## Testing & Validation

### Tutorial Examples
- Located in `tutorial/two_blocks/` directory structure
- Synthetic model: two buried blocks in halfspace
- Complete workflow: mesh → forward → add noise → invert
- Execution: `mpirun -np N e4d` where N = processor count

### Debugging Workflow
1. Check `e4d.log` for detailed execution information
2. Verify environment variables and PATH includes `bin/`
3. Ensure proper MPI fabric: `export I_MPI_FABRICS=shm`
4. Test with single processor for serial debugging

## Common Pitfalls

- Missing Intel environment setup causes linking failures
- Insufficient processors for parallel modes (minimum 2 for inversion)
- TetGen/Triangle executables must be in PATH
- Case-sensitive file extensions (`.cfg`, `.srv`, `.inv`)
- Coordinate system consistency between mesh and survey data
- Memory requirements scale with mesh size and processor count

## Code Style Guidelines

- Fortran 90 modules with explicit interfaces
- Meaningful variable names following physics conventions (`sigma`, `phi`)
- Extensive comments for complex numerical algorithms
- Preprocessor directives for conditional compilation (`#ifdef resmode`)