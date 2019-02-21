# Exareme   [![Build Status](https://travis-ci.org/madgik/exareme.svg?branch=dev_standalone)](https://travis-ci.org/madgik/exareme)


##  Building Exareme

* requires : 
   ```
        -  git, jdk 1.7, maven, consul
   ```

* Package dependencies :
    ```
        - jre 1.7, Python 2.7, requests
        -[APSW 3.11] (https://rogerbinns.github.io/apsw/download.html), NumPy, SciPy, scikit-learn, titus
        (can be installed through pip, also listed in requirements.txt)
    ```

* Open new terminal and execute
    ``` 
        git clone https://github.com/madgik/exareme.git -b dev_standalone exareme-standalone
        cd exareme-standalone
    ```

* Configuration under ~/exareme-standalone/exareme-utils/src/main/resources/gateway.properties where $pwd = your path to exareme-standalone
    ```
    - specify
		demo.repository.path= $pwd/exareme-standalone/mip-algorithms/
		static.path= $pwd/exareme-standalone/runExareme/static/
		workers.path= $pwd/exareme/etc/exareme/workers
		home.path= $pwd
    ```

* Keep in mind, that every time tou make a change under folder ```exareme-standalone``` you have to make
   ```
       mvn clean install (inside exareme-standalone folder)
       mv exareme-distribution/target/exareme/lib/exareme/*.jar runExareme/lib/exareme/
   ```

* Start Consul Key-value store (https://www.consul.io/) ONLY! in Master node
    ```
        consul agent -dev -client 0.0.0.0 -bind=$( wget http://ipinfo.io/ip -qO -)
    ```

* Start Exareme Master node. At this point Exareme will automatically read data from .csv file.
If you want to connect Exareme with a db check section ```Manage DB```
    ```
        cd runExareme
        ./bootstrap.sh --master
    ```
  Note that ```bootstrap.sh``` can have one of two options ```master``` or ```worker```

* Check Consul key-value store. Command line for {yourIP}: ```wget http://ipinfo.io/ip -qO -```
    ```
        {yourIP}:8500
    ```

* For stopping Exareme you can simply go under ```runExareme``` folder and
    ```
        ./bin/exareme-admin.sh --stop
    ```
* Manage DB
#todo

#--------------------------------------------
## Single node installation

* Package dependencies :

    - jre 1.7, Python 2.7, git, [APSW 3.11] (https://rogerbinns.github.io/apsw/download.html)
    - requests, NumPy, SciPy, scikit-learn, titus (can be installed through pip, also listed in requirements.txt)

##TODO, for now algorithms will be inside the fonder
* Download mip-algorithms inside exareme-standalone folder
    ```
    cd exareme-standalone/
    wget https://github.com/madgik/mip-algorithms/archive/master.zip
    unzip master.zip
    ```

* Configuration under ~/mip-algorithms-master/

    - edit properties.json	
        + specify rawdb address, port, credentials, query -if necessary for accessing a db-
      
* Extract Exareme tarball
 
    ```
    mkdir ~/exareme
    tar xf ~/exareme.tar.gz -C ~/exareme
    cd ~/exareme
    ```
    
* Configuration under ~/exareme/

    - Specify master/worker nodes 
    
        ```
        echo $(hostname --ip-address) > etc/exareme/master
        echo "" > etc/exareme/workers
        ```
        
    - Edit the etc/exareme/exareme-env.sh and specify java, python installation (if needed).   
    - Edit the etc/exareme/gateway.properties 
        + specify the mip-algorihtms path (e.g. 
        + specify the gateway port (if needed).

* Start/Stop Exareme and check the logs

    ```
    cd ~/exareme
    ./bin/exareme-admin.sh --start --local
    tail -f /tmp/exareme/var/log/exareme-master.log
    ./bin/exareme-admin.sh --kill --local
    ```

## Cluster installation

* Set up ssh password-less access from master node to all nodes. 
* Install and configure mip-algorithms on each node.
* On master node install Exareme based on single node instructions.
    ```
    cd ~/exareme
    ./bin/exareme-admin.sh --install
    ./bin/exareme-admin.sh --start
    ./bin/exareme-admin.sh --kill
    ```
    
* In order to update configuration files accross nodes
    ```
    ./bin/exareme-admin.sh --update    
    ```
