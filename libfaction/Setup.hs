import Distribution.Simple
main :: IO ()
main = defaultMain

-- Although this looks like the Simple build type, it is in fact vital that
-- we use this Setup.hs because it'll get compiled against the local copy
-- of the Faction lib, thus enabling Faction to bootstrap itself without relying
-- on any previous installation. This also means we can use any new features
-- immediately because we never have to worry about building Faction with an
-- older version of itself.
