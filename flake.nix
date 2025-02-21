{
	description = "Development environment for compli";
	inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

	outputs = { self, nixpkgs }:
		let
			name = "compli";

			supportedSystems = [
				"x86_64-linux"
			];

			forAllSupportedSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
				pkgs = import nixpkgs { inherit system; };
			});
		in
			{
			devShells = forAllSupportedSystems ({ pkgs }: {
				default = pkgs.mkShell.override
					{
						stdenv = pkgs.llvmPackages_18.stdenv;
					}
					{
						inherit name;

						packages = with pkgs; [
							llvmPackages_18.llvm
							libxml2
							libffi
						];
					};
			});
		};
}
