all:
	@echo 'make icons'
	@echo 'make interfaces'
# @echo 'make headers'


icons:patchwork-icon.icns

patchwork-icon.icns:patchwork-icon.iconset
	iconutil --convert icns  patchwork-icon.iconset

# headers:
# 	cd headers32/midishare/C/       ; sh ./populate.sh
# 	cd headers64/midishare/C/       ; sh ./populate.sh
# 	cd headers32/player/C/          ; sh ./populate.sh
# 	cd headers64/player/C/          ; sh ./populate.sh
# 	cd headers32/coreservices/C/    ; sh ./populate.sh
# 	cd headers64/coreservices/C/    ; sh ./populate.sh
# 	cd headers32/coregraphics/C/    ; sh ./populate.sh
# 	cd headers64/coregraphics/C/    ; sh ./populate.sh

#interfaces:
#	ccl -norc < generate-interfaces.lisp
interfaces:
	ccl -norc -l generate-interfaces.lisp -e '(quit)'

.PHONY:icons
