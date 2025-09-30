import os
import subprocess
import shutil
import argparse

# Configuration
CONFIG = {
    "software": {
        "petsc_archive": "petsc-3.21.4.tar.gz",
        "petsc_dir_name": "petsc-3.21.4",
        "triangle_archive": "triangle-1.6.tar.gz",
        "triangle_dir_name": "triangle-1.6",
        "tetgen_archive": "tetgen-1.6.0.tar.gz",
        "tetgen_dir_name": "tetgen-1.6.0",
    },
    "compiler": {
        "CC": "icx",
        "CXX": "icpx",
        "FC": "ifx",
        "MPIFC": "mpiifort -fc=ifx",
        "MPICC": "mpiicc -cc=icx",
        "MPICXX": "mpiicpc -cxx=icpx",
        "FOPTFLAGS": "-O2 -march=native -mtune=native",
        "FDEBUGFLAGS": "-O0 -g -traceback -check bounds,pointers,format",
        "COPTFLAGS": "-O2 -march=native -mtune=native",
        "CXXOPTFLAGS": "-O2 -march=native -mtune=native",
    },
    "directories": {
        "e4d_dir": os.getcwd(),
        "lib_dir": os.path.join(os.getcwd(), "lib"),
        "bin_dir": os.path.join(os.getcwd(), "bin"),
    },
}

# Helper Functions
def is_installed(path):
    """Check if a directory or file exists."""
    return os.path.exists(path)

def run_command(cmd, cwd=None, env=None):
    """Run a shell command with an optional environment."""
    subprocess.run(cmd, shell=True, cwd=cwd, env=env, check=True)

def check_archive_exists(archive_path):
    """Check if the archive file exists in the lib directory."""
    if not os.path.isfile(archive_path):
        print(f"Error: Archive '{archive_path}' not found.")
        print("Ensure the archive is located in the 'lib' directory.")
        exit(1)

def prompt_reinstall(choice, args=None):
    """Ask if the user wants to reinstall each component based on the choice."""
    components = {
        "PETSc": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["petsc_dir_name"]),
        "Triangle": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["triangle_dir_name"]),
        "TetGen": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["tetgen_dir_name"]),
        "E4D-HR": os.path.join(CONFIG["directories"]["bin_dir"], "e4d"),
    }
    
    # Map choice to specific component
    choice_to_component = {
        "1": ["PETSc", "Triangle", "TetGen", "E4D-HR"],  # Install All
        "2": ["PETSc"],
        "3": ["E4D-HR"],
        "4": ["Triangle"],
        "5": ["TetGen"],
        "6": ["E4D-HR"],  # Debug mode
    }
    
    reinstall = {}
    components_to_check = choice_to_component.get(choice, ["PETSc", "Triangle", "TetGen", "E4D-HR"])
    
    for name, path in components.items():
        if name in components_to_check:
            if is_installed(path):
                # Check for command-line arguments first
                if args and getattr(args, 'force', False):
                    reinstall[name] = True
                elif args and getattr(args, 'skip_existing', False):
                    reinstall[name] = False
                else:
                    response = input(f"{name} is already installed. Reinstall? [y/N]: ").strip().lower()
                    reinstall[name] = response == "y"
            else:
                reinstall[name] = True  # Install if not already installed
        else:
            reinstall[name] = False  # Don't install components not selected
    return reinstall

def install_petsc():
    """Install PETSc."""
    petsc_archive = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["petsc_archive"])
    petsc_dir = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["petsc_dir_name"])
    check_archive_exists(petsc_archive)
    shutil.rmtree(petsc_dir, ignore_errors=True)
    print(f"Installing PETSc from {petsc_archive}...")
    shutil.unpack_archive(petsc_archive, CONFIG["directories"]["lib_dir"])
    run_command(
        f"./configure --with-debugging=0 "
        f"--with-fc='{CONFIG['compiler']['MPIFC']}' --with-cc='{CONFIG['compiler']['MPICC']}' "
        f"--with-cxx='{CONFIG['compiler']['MPICXX']}' --with-blaslapack-dir='{os.getenv('MKLROOT')}' "
        f"FOPTFLAGS='{CONFIG['compiler']['FOPTFLAGS']}' COPTFLAGS='{CONFIG['compiler']['COPTFLAGS']}' "
        f"CXXOPTFLAGS='{CONFIG['compiler']['CXXOPTFLAGS']}'",
        cwd=petsc_dir,
    )
    run_command(f"make PETSC_DIR={petsc_dir} PETSC_ARCH=arch-linux-c-opt all", cwd=petsc_dir)
    run_command(f"make PETSC_DIR={petsc_dir} PETSC_ARCH=arch-linux-c-opt check", cwd=petsc_dir)

def install_triangle():
    """Install Triangle."""
    triangle_archive = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["triangle_archive"])
    triangle_dir = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["triangle_dir_name"])
    check_archive_exists(triangle_archive)
    shutil.rmtree(triangle_dir, ignore_errors=True)
    print(f"Installing Triangle from {triangle_archive}...")
    shutil.unpack_archive(triangle_archive, CONFIG["directories"]["lib_dir"])
    run_command(f"make CC={CONFIG['compiler']['CC']}", cwd=triangle_dir)
    shutil.copy(os.path.join(triangle_dir, "triangle"), CONFIG["directories"]["bin_dir"])

def install_tetgen():
    """Install TetGen."""
    tetgen_archive = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["tetgen_archive"])
    tetgen_dir = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["tetgen_dir_name"])
    check_archive_exists(tetgen_archive)
    shutil.rmtree(tetgen_dir, ignore_errors=True)
    print(f"Installing TetGen from {tetgen_archive}...")
    shutil.unpack_archive(tetgen_archive, CONFIG["directories"]["lib_dir"])
    run_command(f"make CXX={CONFIG['compiler']['CXX']}", cwd=tetgen_dir)
    shutil.copy(os.path.join(tetgen_dir, "tetgen"), CONFIG["directories"]["bin_dir"])

def install_e4d_hr(debug=False):
    """Compile and install E4D-HR."""
    petsc_dir = os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["petsc_dir_name"])
    petsc_arch = "arch-linux-c-opt"
    e4d_path = os.path.join(CONFIG["directories"]["bin_dir"], "e4d")
    shutil.rmtree(e4d_path, ignore_errors=True)
    
    mode_str = "debug" if debug else "optimised"
    print(f"Compiling and installing E4D-HR ({mode_str} mode)...")
    if debug:
        print("Note: For optimal debugging, consider using a debug build of PETSc (--with-debugging=1)")
    
    env = os.environ.copy()
    env["PETSC_DIR"] = petsc_dir
    env["PETSC_ARCH"] = petsc_arch
    src_dir = os.path.join(CONFIG["directories"]["e4d_dir"], "src")
    run_command("make clean", cwd=src_dir, env=env)
    
    # Choose flags based on debug mode
    fflags = CONFIG['compiler']['FDEBUGFLAGS'] if debug else CONFIG['compiler']['FOPTFLAGS']
    
    run_command(
        f"make FC='{CONFIG['compiler']['MPIFC']}' "
        f"FFLAGS='{fflags} -r8 -heap-arrays'",
        cwd=src_dir,
        env=env
    )
    shutil.copy(os.path.join(src_dir, "e4d"), e4d_path)

def uninstall():
    """Uninstall all E4D-HR components."""
    print("Uninstalling E4D-HR components...")
    
    components = {
        "PETSc": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["petsc_dir_name"]),
        "Triangle": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["triangle_dir_name"]),
        "TetGen": os.path.join(CONFIG["directories"]["lib_dir"], CONFIG["software"]["tetgen_dir_name"]),
        "E4D-HR binary": os.path.join(CONFIG["directories"]["bin_dir"], "e4d"),
        "Triangle binary": os.path.join(CONFIG["directories"]["bin_dir"], "triangle"),
        "TetGen binary": os.path.join(CONFIG["directories"]["bin_dir"], "tetgen"),
    }
    
    uninstalled_count = 0
    for name, path in components.items():
        if os.path.exists(path):
            try:
                if os.path.isdir(path):
                    shutil.rmtree(path)
                else:
                    os.remove(path)
                print(f"  ✓ Removed {name}")
                uninstalled_count += 1
            except Exception as e:
                print(f"  ✗ Failed to remove {name}: {e}")
        else:
            print(f"  - {name} not found (already uninstalled)")
    
    # Clean up compiled objects in src directory if they exist
    src_dir = os.path.join(CONFIG["directories"]["e4d_dir"], "src")
    if os.path.exists(src_dir):
        try:
            # Remove object files, modules, and executables
            for ext in ["*.o", "*.mod", "e4d", "build_info.inc"]:
                for file_path in os.popen(f"find {src_dir} -name '{ext}' -type f 2>/dev/null").read().strip().split('\n'):
                    if file_path and os.path.exists(file_path):
                        os.remove(file_path)
                        print(f"  ✓ Removed {os.path.basename(file_path)} from src/")
        except Exception as e:
            print(f"  ✗ Failed to clean src directory: {e}")
    
    if uninstalled_count > 0:
        print(f"\nUninstallation completed. Removed {uninstalled_count} components.")
    else:
        print("\nNo components were found to uninstall.")

def menu(args=None):
    """Display menu and return selected option."""
    # Check for numeric shortcuts like -3y (option 3 with yes flag)
    if args and hasattr(args, 'shortcut') and args.shortcut:
        shortcut = args.shortcut
        # Extract option number and optional 'y' flag
        if shortcut[-1] == 'y':
            args.force = True
            choice = shortcut[:-1]
        else:
            choice = shortcut
        
        if choice in ["0", "1", "2", "3", "4", "5", "6"]:
            action_names = {
                '0': 'uninstall all',
                '1': 'install all',
                '2': 'install PETSc',
                '3': 'install E4D-HR',
                '4': 'install Triangle',
                '5': 'install TetGen',
                '6': 'install E4D-HR (debug)'
            }
            print(f"Using shortcut: {action_names[choice]}")
            return choice
    
    # If action is provided via command line, use it
    if args and args.action:
        action_map = {
            'all': '1',
            'petsc': '2', 
            'e4d': '3',
            'triangle': '4',
            'tetgen': '5',
            'e4d-debug': '6',
            'debug': '6',
            'uninstall': '0'
        }
        choice = action_map.get(args.action.lower())
        if choice:
            print(f"Using command-line action: {args.action}")
            return choice
        else:
            print(f"Warning: Unknown action '{args.action}', showing menu instead.")
    
    print("="*40)
    print("E4D-HR Installation Menu:")
    print("="*40)
    print(" 0  Uninstall    All")
    print("[1] Install      All")
    print(" 2  Install      PETSc")
    print(" 3  Install      E4D-HR")
    print(" 4  Install      Triangle")
    print(" 5  Install      TetGen")
    print(" 6  Install      E4D-HR (Debug)")
    print("="*40)
    choice = input("Select an option (0-6, default [1]): ").strip()
    return choice if choice in ["0", "2", "3", "4", "5", "6"] else "1"

def parse_arguments():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="E4D-HR Installation Script",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python install.py                           # Interactive mode
  python install.py --action all             # Install all components
  python install.py --action petsc --force   # Force reinstall PETSc
  python install.py --action e4d --skip      # Install E4D-HR, skip existing
  python install.py --action uninstall --yes # Uninstall without confirmation
  python install.py --action debug           # Install E4D-HR in debug mode
  python install.py -3y                      # Shortcut: Force reinstall E4D-HR
  python install.py -6                       # Shortcut: Install E4D-HR debug
  python install.py -0y                      # Shortcut: Force uninstall all
        """
    )
    
    # Add shortcut arguments for each option
    parser.add_argument('-0', action='store_const', const='0', dest='shortcut', help='Shortcut for uninstall (add y for --yes)')
    parser.add_argument('-1', action='store_const', const='1', dest='shortcut', help='Shortcut for install all (add y for --force)')
    parser.add_argument('-2', action='store_const', const='2', dest='shortcut', help='Shortcut for install PETSc (add y for --force)')
    parser.add_argument('-3', action='store_const', const='3', dest='shortcut', help='Shortcut for install E4D-HR (add y for --force)')
    parser.add_argument('-4', action='store_const', const='4', dest='shortcut', help='Shortcut for install Triangle (add y for --force)')
    parser.add_argument('-5', action='store_const', const='5', dest='shortcut', help='Shortcut for install TetGen (add y for --force)')
    parser.add_argument('-6', action='store_const', const='6', dest='shortcut', help='Shortcut for install E4D-HR debug (add y for --force)')
    
    # Support compound shortcuts like -3y
    parser.add_argument('-0y', action='store_const', const='0y', dest='shortcut', help='Uninstall all without confirmation')
    parser.add_argument('-1y', action='store_const', const='1y', dest='shortcut', help='Force install all')
    parser.add_argument('-2y', action='store_const', const='2y', dest='shortcut', help='Force install PETSc')
    parser.add_argument('-3y', action='store_const', const='3y', dest='shortcut', help='Force install E4D-HR')
    parser.add_argument('-4y', action='store_const', const='4y', dest='shortcut', help='Force install Triangle')
    parser.add_argument('-5y', action='store_const', const='5y', dest='shortcut', help='Force install TetGen')
    parser.add_argument('-6y', action='store_const', const='6y', dest='shortcut', help='Force install E4D-HR debug')
    
    parser.add_argument(
        '--action', '-a',
        choices=['all', 'petsc', 'e4d', 'triangle', 'tetgen', 'e4d-debug', 'debug', 'uninstall'],
        help='Installation action to perform'
    )
    
    parser.add_argument(
        '--force', '-f',
        action='store_true',
        help='Force reinstall existing components without prompting'
    )
    
    parser.add_argument(
        '--skip-existing', '--skip', '-s',
        action='store_true',
        help='Skip existing components without prompting'
    )
    
    parser.add_argument(
        '--yes', '-y',
        action='store_true',
        help='Answer yes to all prompts (e.g., uninstall confirmation)'
    )
    
    return parser.parse_args()

def main():
    args = parse_arguments()
    
    if not os.getenv("MKLROOT"):
        print("Error: MKLROOT is not set. Please set the MKLROOT environment variable.")
        print("Try sourcing the Intel environment variables script, e.g., 'source <intel-compiler-dir>/setvars.sh'")
        return
    os.makedirs(CONFIG["directories"]["lib_dir"], exist_ok=True)
    os.makedirs(CONFIG["directories"]["bin_dir"], exist_ok=True)

    choice = menu(args)
    
    if choice == "0":
        # Confirm uninstall
        if args.yes or (hasattr(args, 'shortcut') and args.shortcut and args.shortcut.endswith('y')):
            confirm = "y"
        else:
            confirm = input("Are you sure you want to uninstall all E4D-HR components? [y/N]: ").strip().lower()
        
        if confirm == "y":
            uninstall()
        else:
            print("\nUninstall cancelled.")
        return
    
    reinstall = prompt_reinstall(choice, args)

    if choice == "1":
        if reinstall["PETSc"]:
            install_petsc()
        if reinstall["Triangle"]:
            install_triangle()
        if reinstall["TetGen"]:
            install_tetgen()
        if reinstall["E4D-HR"]:
            install_e4d_hr()
    elif choice == "2" and reinstall["PETSc"]:
        install_petsc()
    elif choice == "3" and reinstall["E4D-HR"]:
        install_e4d_hr()
    elif choice == "4" and reinstall["Triangle"]:
        install_triangle()
    elif choice == "5" and reinstall["TetGen"]:
        install_tetgen()
    elif choice == "6" and reinstall["E4D-HR"]:
        install_e4d_hr(debug=True)
    print()
    print("="*40)
    print("Installation completed successfully.")
    print("="*40)

if __name__ == "__main__":
    main()
