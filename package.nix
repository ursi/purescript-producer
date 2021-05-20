{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  let inherit (ps-pkgs-ns) ursi; in
  { dependencies =
      [ ursi.prelude
        ursi.refeq
      ];
  }
