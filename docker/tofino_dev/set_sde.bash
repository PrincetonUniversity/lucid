set_sde() {
    export SDE=`pwd`
    export SDE_INSTALL=$SDE/install
    export PATH=$SDE_INSTALL/bin:$PATH
}
set_sde