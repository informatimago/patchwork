all:
	@echo 'make icons'
	@echo 'make headers'
	@echo 'make interfaces'


icons:patchwork-icon.icns

patchwork-icon.icns:patchwork-icon.iconset
	iconutil --convert icns  patchwork-icon.iconset



headers:
	cd headers32/midishare/C/ ; ./populate.sh
	cd headers64/midishare/C/ ; ./populate.sh

interfaces:headers
	ccl -norc < generate-interfaces.lisp
