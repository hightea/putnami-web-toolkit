Putnami Web Toolkit
===================

Travis build status [![Build Status](https://travis-ci.org/Putnami/putnami-web-toolkit.svg?branch=master)](https://travis-ci.org/Putnami/putnami-web-toolkit)

===================

Putnami Web Toolkit (alias PWT) is a web framework made to boost your productivity, and keep your code maintanable.

Based on GWT, PWT provides a strong framework coupled with a nice widget set. With PWT you are a kind of super hero, more productive than ever, you build amazing webapps will be amazing.

The key features of PWT are :

* No more **boilerplate** code, use code is generated
* Efficient **DataBinding**, Define complex view in a single template
* Easy **navigation** with a lightweight MVP framework
* RPC over a native command pattern, for unified the server calls
* Widgets based on [Bootstrap](http://www.getbootstrap.com/) for a clean HTML5

Discover all features on http://pwt.putnami.org

# Usage #

To use PWT please have a look at http://pwt.putnami.org/#!GettingStarted. The documentation provides a full step by step tutorials covering all the key features of PWT.

##Releases##
All our released artifacts are available on the maven central repository. 

[putnami on maven central](http://search.maven.org/#search%7Cga%7C1%7Cfr.putnami).

##Snapshot##
If use need to use the snapshots you need to install the sonatype snapshot repository https://oss.sonatype.org/content/repositories/snapshots/

```
repositories {
	mavenCentral()
	maven{ url 'https://oss.sonatype.org/content/repositories/snapshots/'}
}
```

##Samples##

We provide samples in this repo in the [samples](https://github.com/Putnami/putnami-web-toolkit/tree/master/samples) folder :

In each project, you'll find gradle and maven build files.

You can easily run the samples following those commands :

```
git clone https://github.com/Putnami/putnami-web-toolkit.git
cd putnami-web-toolkit/samples/web-project
gradle jettyRunWar
```

Then browse the sample on http://localhost:8080/pwt-sample-web.

You can have more info on the build tools on the wiki [gradle doc](https://github.com/Putnami/putnami-web-toolkit/wiki/GradleUsage) or [maven doc](https://github.com/Putnami/putnami-web-toolkit/wiki/MavenUsage).


# Build PWT #

**Note :** To build PWT you need gradle. If you are ready to use gradle, please refer to https://www.gradle.org/ to install it.

## Project structure ##

The project is organized that way :

```
.
+-- core    => contains the core framework project
+-- doc     => contains the documentation webapp project
+-- plugins => contains the provided plugins projects
+-- samples => contains the samples projects
+-- settings => contains the settings and workspace materials
+-- tutorials => contains the step by step tutorials
```
The samples projects are not children of the root project. They have to be build separately.

## Commands ##

Start to clone the git repository

```
git clone https://github.com/Putnami/putnami-web-toolkit.git
cd putnami-web-toolkit
```

To build the framework by yourself, it's very simple:

```
gradle build
```

Then all the built artifacts are available in the build/libs of each sub projects

**Note :** All the commands can be lunch from on specific sub project eg. ./core

Useful gradle tasks:

* **gradle install** => publish all artifacts into your local maven repository
* **gradle licenseFormatAll** => Check and add if necessary the license header in the source files

You can also run webapps such as ./doc or ./samples/web-project with the commands

* **gradle gwtDev** => run the webapp with the GWT SDM (super dev mode). It's the easiest way to develop => [http://localhost:8888/](http://localhost:8888/)
* **gradle jettyRunWar** => build the war and run jetty => [http://localhost:8080/](http://localhost:8080/)
* **gradle jettyDraftWar** => build the war in draft mode and run jetty (faster than jettyRunWar) => [http://localhost:8080/](http://localhost:8080/)

# Help and Contribute #
We need you!
Any help is welcome. And there is many ways to help us:

## Be a nice community member ##
If you tried and you love PWT. We will be glad to count you as community members. So please :

* Star this project and become one famous [stargazers](https://github.com/Putnami/putnami-web-toolkit/stargazers)
* Become a follower on [twitter](https://twitter.com/PutnamiTeam)
* Join the news group [putnami-web-toolkit](https://groups.google.com/forum/#!forum/putnami-web-toolkit)

Of course you will get all our thankfulness if you blog, share and spread PWT around you. All the backlinks on [http://putnami.org](http://putnami.org) are very welcome.

## Report issues ##
All issues about bugs or enhancement are precious and will be given careful consideration. We're going to help you in the best delay as can (generally in the day time).

To report an issue, please use the project [issue tracker](https://github.com/Putnami/putnami-web-toolkit/issues)

## Contribute ##

You'd love to contribute your code, nice :)
First be sure that your code respect the project code style and formating. 
All the documentation is [here](https://github.com/Putnami/putnami-web-toolkit/blob/master/settings/README.md)

If you are an eclipse user, it could be nice if you follow the workspace setup instructions.

Every pullrequest will be review with a great consideration, and with a full open mind. 


# License
The framework is delivered under LGPL v 3.0.

The LGPL v 3.0 allows a free usage of PWT for commercial and open source projects.
We equally guarantees that PWT is and will open source for ever.

PWT doesn't affect the license of your application. Using PWT is free of charge so fill free to use and integrate it.

You can have a look at the licence details on a https://www.gnu.org/licenses/lgpl-3.0.txt.


---

---

We hope that this framework will help you to build great apps. Best regards.

[@PutnamiTeam](https://github.com/putnami)
