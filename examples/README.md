# Loup Examples

This directory contains example configuration for running loup.

### Install Loup

If not installed, install loup through hackage:

```bash
$ stack install --resolver lts-6.30 loup
```

Otherwise, install from source.

### Register the workflows with SWF

If the workflows have not been registered yet, register them with:

```bash
$ ./register.hs --plan plan.yaml --domain pools
```

This will through up warnings if workflows have already been registered.

### Run a decider(s)

Run a decider(s) to make pool decisions for the workers:

```bash
$ loup-decider --domain pools --plan plan.yaml
```

This will run continuously in a loop.

### Run a actor(s)

Run a actor(s) to run work out of the pool:

```bash
$ loup-actor --domain pools --queue activity --command ./hello.sh --interval 5
```

This will run continuously in a loop.

### Run the converger

Run the converger to start, stop work:

```bash
$ loup-converger --domain pools --pool pool.yaml 
```

This will run the jobs in pool.yaml with the configured inputs. Jobs not in
pool.yaml will be stopped. Jobs not running in pool.yaml will be started.
