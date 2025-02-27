HOME := "$HOME"
HS_FILES := "$(git ls-files '*.hs' '*.hs-boot')"
CHANGED_HS_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep ".*\.hs$")'
NIX_FILES := "$(git ls-files '*.nix' 'nix/*.nix')"
SHELL_FILES := "$(git ls-files '*.sh')"
CHANGED_SHELL_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep ".*\.sh$$")'

NIX_FMT := "nix fmt"
ORMOLU := "ormolu"
ORMOLU_VERSION := "$(" + ORMOLU + " --version | awk 'NR==1 { print $2 }')"
ORMOLU_CHECK_VERSION := "0.7.2.0"

# Run Shellcheck with access to any file that's sourced, relative to the script's own directory
SHELLCHECK := "$(shellcheck --external-sources --source-path=SCRIPTDIR)"

#-------------------------------------------------------------------------------
## Cabal

# Run the backend service
run:
  cabal run exe:server

# Build all haskell packages.
build:
  cabal build all --enable-tests --enable-benchmarks

# Delete all build artifacts.
clean:
  cabal clean

# Run all test suites.
test: 
  cabal test all --test-show-details=direct --test-option=--format=specdoc

# Build docs
haddock: 
  cabal haddock

#-------------------------------------------------------------------------------
## Formatting

check-ormolu-version:
  @if ! [ "{{ORMOLU_VERSION}}" = "{{ORMOLU_CHECK_VERSION}}" ]; then \
    echo "WARNING: ormolu version mismatch, expected {{ORMOLU_CHECK_VERSION}} but got {{ORMOLU_VERSION}}"; \
  fi

# Auto-format Haskell source code using ormolu.
format-hs: check-ormolu-version
  @echo running {{ORMOLU}} --mode inplace
  @{{ORMOLU}} --mode inplace {{HS_FILES}}

# Auto-format Haskell source code using ormolu (changed files only).
format-hs-changed:
  @echo running {{ORMOLU}} --mode inplace
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	{{ORMOLU}} --mode inplace {{CHANGED_HS_FILES}}; \
  fi

# Check Haskell source code formatting using ormolu.
check-format-hs: check-ormolu-version
  @echo running {{ORMOLU}} --mode check
  @{{ORMOLU}} --mode check {{HS_FILES}}

# Check Haskell source code formatting using ormolu (changed-files-only).
check-format-hs-changed: check-ormolu-version
  @echo running {{ORMOLU}} --mode check
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	{{ORMOLU}} --mode check {{CHANGED_HS_FILES}}; \
  fi

# Auto-format Nix source code using `nixpkgs-fmt`.
format-nix:
  @if command -v {{NIX_FMT}} > /dev/null; then \
    echo "running {{NIX_FMT}}"; \
    {{NIX_FMT}} {{NIX_FILES}}; \
  else \
    echo "{{NIX_FMT}} is not installed; skipping"; \
  fi

# Check Nix source code using `nixpkgs-fmt`.
check-format-nix:
  @if command -v {{NIX_FMT}} > /dev/null; then \
  	echo "running {{NIX_FMT}} --check"; \
  	{{NIX_FMT}} --check {{NIX_FILES}}; \
  else \
  	echo "{{NIX_FMT}} is not installed; skipping"; \
  fi


# Run all formatters.
format: format-hs format-nix

# Run all formatters on changed files.
format-changed: format-hs-changed format-nix

# Check formatting on all files.
check-format: check-format-hs check-format-nix

# Check formatting on all changed files.
check-format-changed: check-format-hs-changed check-format-nix

# Lint shell scripts using `shellcheck`.
lint-shell:
  @echo running shellcheck
  @{{SHELLCHECK}} {{SHELL_FILES}}

# Lint shell scripts using `shellcheck` (changed files only).
lint-shell-changed:
  @echo running shellcheck
  @if [ -n "{{CHANGED_SHELL_FILES}}" ]; then \
  	{{SHELLCHECK}} {{CHANGED_SHELL_FILES}}; \
  fi
