Putnami Web Toolkit
===================

[![Build Status](https://travis-ci.org/Putnami/putnami-pwt.svg?branch=master)](https://travis-ci.org/Putnami/putnami-pwt)

Putnami Web Toolkit is a [GWT](http://www.gwtproject.org/) based framework, providing nice graphical components (based on [Bootstrap](http://www.getbootstrap.com/)), and a bunch of nice features like data binding, unified server calls, etc.

The **main goal** of this framework is to improve your productivity by removing unnecessary, useless code. The framework will then generate what's necessary.

# Resources
Useful links :

* [Documentation](http://pwt.putnami.org/?utm_source=github-pwt)
* [Getting started with the framework](http://pwt.putnami.org/?utm_source=github-pwt#!GettingStarted)
* [Release repository](http://repo.putnami.org/)
* [Download page](http://pwt.putnami.org/?utm_source=github-pwt#!Download)

# License
The framework is delivered under LGPL v 3.0.

This license allows the use of PWT in both open source and commercial projects, and guarantees that this framework and any modification made to it will stay open source.

The use of PWT in your application doesn't and will not affect the license of you application. Using PWT is free of charge and you won't have to pay any fees to use or integrate it.

You can find a [copy](https://github.com/Putnami/putnami-pwt/raw/master/COPYING) of the GPL v3 or you can get the [original](https://www.gnu.org/licenses/gpl-3.0.txt) and a [copy](https://github.com/Putnami/putnami-pwt/raw/master/COPYING.LESSER) of the LGPL v3 or you can get the [orignal](https://www.gnu.org/licenses/lgpl-3.0.txt).

# Usage #
To use this framework, simply add a dependency to the core jar (*pwt-core.jar*) and eventually to the needed plugin jars (*pwt-code-editor.jar* and/or *pwt-google-analytics.jar*).

We release those jar on our maven repository hosted on github reachable via the following URL : http://repo.putnami.org

We provide two sample apps in this repo in the [samples](https://github.com/Putnami/putnami-pwt/tree/master/samples) folder :

* One is a GWT library using PWT
* One is a GWT Web application using PWT

In each project, you'll find gradle and maven build files.

You can easily run the samples following those commands :

```
git clone git@github.com:Putnami/putnami-pwt.git
cd putnami-pwt/samples/web-project
gradle jettyRunWar
Then browse the sample on http://localhost:8080/pwt-sample-web.
```
Gradle users please have a look on this [page](https://github.com/Putnami/putnami-pwt/wiki/GradleUsage).

Maven users please have a look on this [page](https://github.com/Putnami/putnami-pwt/wiki/MavenUsage).

# Build #
We use Gradle to build our framework.

The project is organized that way :

```
.
+-- core    => contains the core framework project
+-- doc     => contains the documentation webapp project
+-- samples => contains the samples projects
+-- plugins => contains the provided plugins projects
```
The samples projects are not children of the root project. They ave to be build separately.

On the repository root we provide some useful tasks :

* **publishAll** => publish all sub-projects to the defined maven repo
* **publishAllToMavenLocal** => publish all sub-projects to the local maven repo
* **publishAllLibs** => publish all non webApp sub-projects (today all projects but doc) to the defined maven repo
* **publishAllLibsToMavenLocal** => publish all non webApp sub-projects (today all projects but doc) to the local maven repo
* **licenseAll** => Check the license header in the files of all sub-projects
* **licenseFormatAll** => Check and add if necessary the license header in the files of all sub-projects

All the publishing tasks depends on the *licenseFormat* task : publishing a project always check and update update if needed the license headers.


If you want to do build the framework yourself, it's very simple :

* Use `gradle jar` on the repository root to generate the jars, which will be in the project's build/libs folder.
* Use `gradle jar` on the project root to generate the project jar, which will be in the project's build/libs folder.
* Use `gradle publishAllLibsToMavenLocal` on the repository root to publish all projects except doc in your local maven repo.

# Contribute #
If you want to contribute to this project, you can report issues or submit your ideas for new amazing features [here](https://github.com/Putnami/putnami-pwt/issues)

We're unfortunatly not ready to accept pullrequest. All the docs and tools concerning code style, code formating, commit pattern is still missing.


---

---

We hope that this framework will help you to build great apps. Best regards.

[@Putnami team](https://github.com/putnami)
