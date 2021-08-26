SDE_DIR=~/bf-sde-9.5.0
if [ -e bf-sde-9.5.0.tgz ]
then 
  # install bf-sde-9.5.0.tgz
  tar -xzf bf-sde-9.5.0.tgz -C ~
  echo "vm_sde_profile:
    global_configure_options: ''
    package_dependencies:
    - thrift
    - grpc
    packages:
    - bf-syslibs:
      - bf_syslibs_configure_options: ''
    - bf-utils:
      - bf_utils_configure_options: ''
    - bf-drivers:
      - bf_drivers_configure_options: ''
      - bf-runtime
      - p4-runtime
      - pi
    - ptf-modules
    tofino_architecture: tofino" > "$SDE_DIR"/p4studio_build/profiles/vm_sde_profile.yaml
  cd "$SDE_DIR"
  ./p4studio_build/p4studio_build.py -up vm_sde_profile
  echo ". $SDE_DIR/set_sde.sh" | ~/.bashrc
  cd -
else
  echo "not installing p4 studo SDE: bf-sde-9.5.0.tgz not found"
fi