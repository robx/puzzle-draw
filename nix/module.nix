{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
with lib; let
  cfg = config.services.puzzle-draw;
in {
  options.services.puzzle-draw = {
    enable = mkEnableOption "puzzle-draw server";

    hostName = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Hostname to serve puzzle-draw virtual host on (null to disable)";
    };

    nginx = mkOption {
      type =
        types.submodule
        (import "${modulesPath}/services/web-servers/nginx/vhost-options.nix" {
          inherit config lib;
        });
      default = {};
      description = "Extra configuration for the nginx virtual host of puzzle-draw.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.puzzle-draw = {
      description = "Run puzzle-draw server";
      wantedBy = ["multi-user.target"];
      after = ["networking.target"];
      serviceConfig = {
        WorkingDirectory = "${pkgs.puzzle-draw-serve}";
        ExecStart = "${pkgs.puzzle-draw-serve}/bin/servepuzzle -b 127.0.0.1 -p 8765";
        Restart = "always";
      };
    };
    services.nginx = {
      upstreams.puzzle-draw-backend.servers."localhost:8765" = {};
      virtualHosts = mkIf (cfg.hostName != null) {
        "${cfg.hostName}" = mkMerge [
          cfg.nginx
          {
            locations = {
              "/" = {
                proxyPass = "http://puzzle-draw-backend/";
              };
            };
          }
        ];
      };
    };
  };
}
