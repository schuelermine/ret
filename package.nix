{ mkDerivation, base, case-insensitive, containers, directory
, filepath, lib, process, unix
}:
mkDerivation {
  pname = "ret";
  version = "0.1.2.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base case-insensitive containers directory filepath process unix
  ];
  description = "A tool that returns to a landmark parent directory";
  license = lib.licenses.mit;
}
