# Step 1: Nix

On a linux system (including Windows Subsystem For Linux), choose the appropriate bash install command for your operating system from [here](https://nixos.org/download/).

# Step 2: Obelisk

Obelisk explains this pretty well [here](https://github.com/Ace-Interview-Prep/obelisk?tab=readme-ov-file#installing-obelisk)

# Step 3: Jenga/Check Installation

Jenga setup is managed through a fork of Obelisk, so as long as Obelisk is installed correctly, Jenga should install and build perfectly fine.

As a core feature of Nix we only build if we would produce a new build; if we have not changed a dependency then we do not need to rebuild it, which nix handles for us. We simply build as we would anytime:

```bash
ob run
```

since this is our first time running Jenga, it may just take some time. In the future there will be more support for a Jenga-specific [nix binary cache](https://zero-to-nix.com/concepts/caching/) to make setup quicker.
