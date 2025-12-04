# Gloom

A statically-typed interpreted language with two-phase execution.

## 🚀 Quick Start

### Option 1: Nix Shell (Recommended)

If you have Nix installed, get a complete development environment instantly:

```bash
cabal build && nix-shell
```

This provides:
- ✅ GHC, Cabal, and Haskell Language Server
- ✅ Neovim preconfigured with Gloom syntax highlighting
- ✅ Auto-compilation on shell entry
- ✅ `gloom` command ready to use

### Option 2: Manual Setup (Development)

For active development, use the setup script:

```bash
source ./setup-env.sh
```

This will:
1. Compile the project with `cabal build`
2. Create a local `bin/` directory with symlink
3. Add `bin/` to your PATH (temporary)
4. Export useful environment variables

**Important:** Use `source` or `.` to execute in the current shell, not `./setup-env.sh`.

Verify installation:
```bash
gloom --version
# Gloom v0.1.0 (experimental)

which gloom
# /path/to/bird/bin/gloom
```

**Note:** You'll need to run `source ./setup-env.sh` in each new terminal session.

### Option 3: Direct Compilation (Windows/Any OS)

If you just want to compile and run without scripts:

```bash
# 1. Build the project
cabal build

# 2. Find the executable location
cabal exec which haskell
# Output example: /path/to/bird/dist-newstyle/build/.../haskell

# 3. Run directly
cabal run haskell -- your-program.gloom
```

Or copy the executable to current directory for convenience:

```bash
cabal build
cp $(cabal list-bin haskell) ./gloom.exe  # Windows: gloom.exe, Linux/Mac: gloom
./gloom.exe your-program.gloom            # Now you can run it directly
```

**Tip for Windows users:** After `cabal build`, the executable is in:
```
dist-newstyle\build\x86_64-windows\ghc-X.X.X\bird-0.1.0.0\x\haskell\build\haskell\haskell.exe
```

You can copy it to your project folder or add it to PATH.

---

**For detailed language documentation, see the separate PDF guide (coming soon).**


