
wasm:
	cd vm && wasm-pack build --out-dir ../www/pkg --no-pack --no-typescript --target=web

doc:
	pandoc -i README.md -o www/readme.html

