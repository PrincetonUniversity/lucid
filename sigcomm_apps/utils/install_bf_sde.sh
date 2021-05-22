# install bf-sde-9.5.0.tar
tar -xf /vagrant/bf-sde-9.5.0.tar -C ~
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
  tofino_architecture: tofino" > ~/bf-sde-9.5.0/p4studio_build/profiles/vm_sde_profile.yaml
cd ~/bf-sde-9.5.0
./p4studio_build/p4studio_build.py -up vm_sde_profile
cd -
