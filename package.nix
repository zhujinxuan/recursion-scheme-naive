let
  lib =
    { src = ./src;
      dependencies = [ "containers" "recursion-schemes" "text"];
    };
in
  { main = "Main";
    src = ./app;
    packages = [ lib ];
  }
