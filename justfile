elm_dirs := "src tests"

[private, default]
default:
  @just --list --unsorted

format:
  elm-format {{elm_dirs}} --yes

check-format:
  elm-format {{elm_dirs}} --validate

test:
  elm-test

check-flake:
  nix flake check -L

verify: check-format test check-flake

clean:
  rm -rf elm-stuff

alias f := format
alias t := test
alias v := verify
alias c := clean
