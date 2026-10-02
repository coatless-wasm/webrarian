# the key user-facing errors read as they should

    Code
      check_collection("no-such-collection")
    Condition
      Error:
      ! Not a webrarian collection
      i No '_webrarian.yml' found in 'no-such-collection' or its parent directories
      i Run `webrarian::catalog()` to create a new collection

---

    Code
      config_keys_to_snake(list(files = list(mount_point = "/x")))
    Condition
      Error in `config_keys_to_snake()`:
      ! Config key files.mount_point uses an underscore.
      i Keys in '_webrarian.yml' are lowercase with hyphens: mount-point.

---

    Code
      check_requirements(root)
    Condition
      Error in `check_requirements()`:
      ! Docker is installed but not running.
      i Start Docker Desktop (or the Docker daemon), then run `bind()` again.
      i Compiling local and GitHub packages to WebAssembly needs it.

