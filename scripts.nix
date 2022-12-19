{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:howto-generics' --allow-eval --warnings";
  allScripts = [ghcidScript];
}
