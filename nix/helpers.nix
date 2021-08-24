self: super:

let
  pkgs = self.pkgs;

  mk-kk-logs = env:
    super.pkgs.writeShellScriptBin "kk-logs--${env}" ''
      log=$(${pkgs.awslogs}/bin/awslogs groups --profile "moixa-${env}" | ${pkgs.fzf}/bin/fzf)
      echo "''${log}"
      while true; do
        seq 2
        ${pkgs.awslogs}/bin/awslogs get --profile "moixa-${env}" "''${@:--w}" "''${log}"
        echo -n "Again ''${log}? "
        read -r
      done
    '';

  mk-list-user-pools = env:
    super.pkgs.writeShellScriptBin "kk-list-user-pools--${env}" ''
      ${pkgs.awscli}/bin/aws cognito-idp list-user-pools \
        --profile "moixa-${env}" \
        --max-results 10 \
        | ${pkgs.jq}/bin/jq --raw-output '.UserPools[] | .Id + " -> " + .Name'
    '';

  cleanEmitted = super.pkgs.writeShellScriptBin "clean-emitted" ''
        echo "> cleaning dependencies"

        for DEP_TSCONFIG in $(${pkgs.jq}/bin/jq --raw-output '.references[] | .path' tsconfig.json)
        do
          DEP_DIR=$(dirname "''${DEP_TSCONFIG:?}")
          DEP_EMITTED=$(${pkgs.jq}/bin/jq --raw-output '.compilerOptions.outDir' "''${DEP_TSCONFIG:?}")
          echo ">   rm -r ''${DEP_DIR:?}/''${DEP_EMITTED:?}"
          rm -r "''${DEP_DIR:?}/''${DEP_EMITTED:?}"
        done

        EMITTED=$(${pkgs.jq}/bin/jq --raw-output '.compilerOptions.outDir' tsconfig.json)
        echo ">   rm -r ''${EMITTED:?}"
        rm -r "''${EMITTED:?}"

        echo "> done"
    '';

in {
  inherit cleanEmitted;

  kk-logs--dev = mk-kk-logs "dev";
  kk-logs--prod = mk-kk-logs "prod";

  kk-list-user-pools--dev = mk-list-user-pools "dev";
  kk-list-user-pools--prod = mk-list-user-pools "prod";
}
