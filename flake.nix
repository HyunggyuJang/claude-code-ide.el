{
  description = "Claude Code IDE Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Core development tools
            websocat          # WebSocket proxy for debugging
            jq                # JSON processing for log analysis
            lsof              # Port monitoring
            
            # Development utilities
            curl              # HTTP testing
            netcat            # Network debugging
            socat             # Socket utilities

            # Shell utilities
            bash              # Ensure consistent bash version
            coreutils         # Standard utilities (grep, etc.)
            procps            # Process utilities (kill, ps)
          ];

          shellHook = ''
            echo "🚀 Claude Code IDE Development Environment"
            echo ""
            echo "Available tools:"
            echo "  📡 websocat   - WebSocket proxy for debugging MCP messages"
            echo "  🔍 jq         - JSON processing and analysis"
            echo "  🔧 lsof       - Port and process monitoring"
            echo "  📋 curl/nc    - Network testing utilities"
            echo ""
            echo "Usage:"
            echo "  ./record-claude-messages.sh [directory]  # Debug MCP messages"
            echo "  emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit  # Run tests"
            echo ""
            
            # Verify websocat installation
            if command -v websocat >/dev/null 2>&1; then
              echo "✅ websocat version: $(websocat --version)"
            else
              echo "❌ websocat not found"
            fi
            
            # Check for claude CLI
            if command -v claude >/dev/null 2>&1; then
              echo "✅ claude CLI available"
            else
              echo "⚠️  claude CLI not found - install separately if needed"
            fi
            
            # Check if Emacs is running with claude-code-ide
            if [ -d "$HOME/.claude/ide" ] && ls "$HOME/.claude/ide"/*.lock >/dev/null 2>&1; then
              echo "✅ Emacs MCP server lockfiles found:"
              ls -la "$HOME/.claude/ide"/*.lock | while read -r line; do
                lockfile=$(echo "$line" | awk '{print $NF}')
                if grep -q "Emacs" "$lockfile" 2>/dev/null; then
                  port=$(basename "$lockfile" .lock)
                  echo "    📡 Port $port (Emacs)"
                fi
              done
            else
              echo "⚠️  No Emacs MCP server lockfiles found"
              echo "    Run 'claude-code-ide' in Emacs to start MCP server"
            fi
            
            echo ""
            echo "🎯 Ready for claude-code-ide development!"
          '';

          # Environment variables for development
          FORCE_COLOR = "1";
          TERM = "xterm-256color";
        };
      });
}
