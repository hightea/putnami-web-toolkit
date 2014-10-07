/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.mvp.rebind;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.gwt.activity.shared.Activity;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.RunAsyncCallback;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JConstructor;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.io.PrintWriter;
import java.util.List;

import fr.putnami.pwt.core.mvp.client.ViewActivity;
import fr.putnami.pwt.core.mvp.client.ViewDecorator;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription.Scope;
import fr.putnami.pwt.core.mvp.client.exception.ApplicationUnreachableException;

public class ProxyViewCreator {

	private static final String PROXY_SUFFIX = "_ProxyView";

	private final JClassType placeType;

	private final String viewProxyQualifiedName;
	private final String viewProxySimpleName;
	private final String packageName;
	private final ActivityDescription activityDescrition;
	private Class<? extends PlaceTokenizer> placeTokenizerClass;
	private Class<? extends ViewDecorator> viewDecoratorClass;

	public ProxyViewCreator(JClassType placeType) {
		this.placeType = placeType;
		this.packageName = this.placeType.getPackage().getName();
		String qualifiedName = this.placeType.getQualifiedSourceName() + ProxyViewCreator.PROXY_SUFFIX;
		this.viewProxyQualifiedName =
				qualifiedName.replace(placeType.getName(), placeType.getName().replace('.', '_'));
		this.viewProxySimpleName = this.viewProxyQualifiedName.replace(this.packageName + ".", "");
		this.activityDescrition = placeType.getAnnotation(ActivityDescription.class);
		this.placeTokenizerClass = this.activityDescrition.placeTokenizer();

		if (PlaceTokenizer.class.equals(this.placeTokenizerClass)) {
			this.placeTokenizerClass = null;
			this.getClass();
			try {
				Class<? extends ViewPlace> placeClass =
						(Class<? extends ViewPlace>) Class.forName(this.placeType.getQualifiedSourceName());
				for (Class<?> inter : placeClass.getInterfaces()) {
					if (inter.equals(PlaceTokenizer.class)) {
						this.placeTokenizerClass = (Class<? extends PlaceTokenizer<?>>) placeClass;
					}
				}
			} catch (ClassNotFoundException e) {
				this.placeTokenizerClass = null;
			}
		}
		this.viewDecoratorClass = this.activityDescrition.viewDecorator();
		if (ViewDecorator.class.equals(this.viewDecoratorClass)) {
			this.viewDecoratorClass = null;
		}
	}

	public String create(TreeLogger logger, GeneratorContext context) {
		PrintWriter printWriter = context.tryCreate(logger, this.packageName, this.viewProxySimpleName);
		if (printWriter == null) {
			return this.viewProxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(printWriter, context);

		srcWriter.indent();
		this.generateProxy(logger, srcWriter);
		srcWriter.println();
		this.generateTokenPrefixes(logger, srcWriter);
		srcWriter.println();
		if (this.placeTokenizerClass == null) {
			this.generateInternalTokenizer(logger, srcWriter);
		} else {
			this.generateDelegateTokenizer(logger, srcWriter);
		}
		this.generateActivityFactory(logger, srcWriter);
		srcWriter.outdent();

		srcWriter.commit(logger);
		return this.viewProxyQualifiedName;
	}

	private void generateActivityFactory(TreeLogger logger, SourceWriter srcWriter) {
		srcWriter.println("@Override");
		srcWriter.println("public Activity createActivity(Place place) {");
		srcWriter.indent();
		srcWriter.println("return new ViewActivity(this, place);");
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateInternalTokenizer(TreeLogger logger, SourceWriter srcWriter) {
		boolean hasTokeConstructor = false;
		for (JConstructor constructor : this.placeType.getConstructors()) {
			if (constructor.getParameters().length == 1
					&& constructor.getParameters()[0].getType().getSimpleSourceName().equals(
							String.class.getSimpleName())) {
				hasTokeConstructor = true;
			}
		}
		srcWriter.println("@Override");
		srcWriter.println("public %s getPlace(String token) {", this.placeType.getSimpleSourceName());
		srcWriter.indent();
		if (hasTokeConstructor) {
			srcWriter.println("return new %s(token);", this.placeType.getSimpleSourceName());
		} else {
			srcWriter.println("%s place = new %s();", this.placeType.getSimpleSourceName(),
					this.placeType.getSimpleSourceName());
			srcWriter.println("place.setToken(token);");
			srcWriter.println("return place;");
		}
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("@Override");
		srcWriter.println("public String getToken(%s place) {", this.placeType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("if(place instanceof ViewPlace){");
		srcWriter.indent();
		srcWriter.println("return ((ViewPlace)place).getToken();");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("return null;");
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateDelegateTokenizer(TreeLogger logger, SourceWriter srcWriter) {
		srcWriter.println("@Override");
		srcWriter.println("public %s getPlace(String token) {", this.placeType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("return new %s().getPlace(token);", this.placeTokenizerClass.getSimpleName());
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("@Override");
		srcWriter.println("public String getToken(%s place) {", this.placeType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("return new %s().getToken(place);", this.placeTokenizerClass.getSimpleName());
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateTokenPrefixes(TreeLogger logger, SourceWriter srcWriter) {
		List<String> tokens = Lists.newArrayList();
		tokens.add("\"" + this.placeType.getSimpleSourceName().replaceAll("Place$", "") + "\"");
		for (String alias : this.activityDescrition.aliases()) {
			tokens.add("\"" + alias + "\"");
		}

		srcWriter.println("@Override");
		srcWriter.println("public String[] getPlacePrefixes(){");
		srcWriter.indent();
		srcWriter.println("return new String[]{ %s };", Joiner.on(", ").join(tokens));
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateProxy(TreeLogger logger, SourceWriter srcWriter) {

		String viewName = this.activityDescrition.view().getSimpleName();
		srcWriter.println();
		srcWriter.println("private static %s view;", viewName);
		srcWriter.println();
		srcWriter.println("@Override");
		srcWriter.println("public void loadView(final ViewProxy.Callback callback) {");
		srcWriter.indent();
		if (this.activityDescrition.asyncView()) {
			srcWriter.println("GWT.runAsync(%s.class, new RunAsyncCallback() {", viewName);
			srcWriter.indent();
			srcWriter.println("public void onFailure(Throwable reason) {");
			srcWriter.indent();
			srcWriter
			.println("if (ApplicationUnreachableException.HTTP_DOWNLOAD_FAILURE_EXCEPTION.equals(reason.getClass().getSimpleName())) {");
			srcWriter.indent();
			srcWriter.println("reason = new ApplicationUnreachableException(reason);");
			srcWriter.outdent();
			srcWriter.println("}");
			srcWriter.println("GWT.reportUncaughtException(reason);");
			srcWriter.outdent();
			srcWriter.println("}");
			srcWriter.println("public void onSuccess() {");
			srcWriter.indent();
			srcWriter.println("if(view == null || %s){",
					this.activityDescrition.scope() == Scope.PROTOTYPE);
			srcWriter.indent();
			srcWriter.println("view = GWT.create(%s.class);", viewName);
			srcWriter.outdent();
			srcWriter.println("}");
			this.generateProxyResult(logger, srcWriter);
			srcWriter.outdent();
			srcWriter.println("}");
			srcWriter.outdent();
			srcWriter.println("});");
		} else {
			srcWriter.println("if(view == null || %s){",
					this.activityDescrition.scope() == Scope.PROTOTYPE);
			srcWriter.indent();
			srcWriter.println("view = GWT.create(%s.class);", viewName);
			srcWriter.outdent();
			srcWriter.println("}");
			this.generateProxyResult(logger, srcWriter);
		}
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateProxyResult(TreeLogger logger, SourceWriter srcWriter) {
		if (this.viewDecoratorClass == null) {
			srcWriter.println("callback.showView(view);");
		} else {
			String decoratorName = this.viewDecoratorClass.getSimpleName();
			srcWriter.println("%s decorator = %s.get();", decoratorName, decoratorName);
			srcWriter.println("decorator.setView(view);");
			srcWriter.println("callback.showView(decorator);");
		}
	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {
		ClassSourceFileComposerFactory composerFactory =
				new ClassSourceFileComposerFactory(this.packageName, this.viewProxySimpleName);

		composerFactory.setSuperclass(this.placeType.getSimpleSourceName());

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(RunAsyncCallback.class.getName());
		composerFactory.addImport(ViewProxy.class.getName());
		composerFactory.addImport(Place.class.getName());
		composerFactory.addImport(ViewPlace.class.getName());
		composerFactory.addImport(Activity.class.getName());
		composerFactory.addImport(ViewActivity.class.getName());
		composerFactory.addImport(ApplicationUnreachableException.class.getName());
		composerFactory.addImport(this.placeType.getQualifiedSourceName());
		composerFactory.addImport(this.activityDescrition.view().getCanonicalName());
		if (this.placeTokenizerClass != null) {
			composerFactory.addImport(this.placeTokenizerClass.getCanonicalName());
		}
		if (this.viewDecoratorClass != null) {
			composerFactory.addImport(this.viewDecoratorClass.getCanonicalName());
		}

		composerFactory.addImplementedInterface(ViewProxy.class.getSimpleName() + "<"
				+ this.placeType.getSimpleSourceName() + ">");

		return composerFactory.createSourceWriter(ctx, printWriter);
	}
}
