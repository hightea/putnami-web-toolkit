/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.mvp.rebind;

import java.io.PrintWriter;
import java.util.List;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.gwt.activity.shared.Activity;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.RunAsyncCallback;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JConstructor;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.mvp.client.ViewActivity;
import fr.putnami.pwt.core.mvp.client.ViewDecorator;
import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription.Scope;

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
		this.viewProxyQualifiedName = this.placeType.getQualifiedSourceName() + ProxyViewCreator.PROXY_SUFFIX;
		this.viewProxySimpleName = this.placeType.getSimpleSourceName() + ProxyViewCreator.PROXY_SUFFIX;
		this.activityDescrition = placeType.getAnnotation(ActivityDescription.class);
		this.placeTokenizerClass = activityDescrition.placeTokenizer();
		if (PlaceTokenizer.class.equals(placeTokenizerClass)) {
			placeTokenizerClass = null;
			try {
				Class<? extends ViewPlace> placeClass = (Class<? extends ViewPlace>) getClass().forName(this.placeType.getQualifiedSourceName());
				for (Class<?> inter : placeClass.getInterfaces()) {
					if (inter.equals(PlaceTokenizer.class)) {
						placeTokenizerClass = (Class<? extends PlaceTokenizer<?>>) placeClass;
					}
				}
			}
			catch (ClassNotFoundException e) {
				// Nothing to do
			}
		}
		this.viewDecoratorClass = activityDescrition.viewDecorator();
		if (ViewDecorator.class.equals(viewDecoratorClass)) {
			viewDecoratorClass = null;
		}
	}

	public String create(TreeLogger logger, GeneratorContext context) throws UnableToCompleteException {
		String packageName = this.placeType.getPackage().getName();
		String className = this.viewProxySimpleName;
		PrintWriter printWriter = context.tryCreate(logger, packageName, className);
		if (printWriter == null) {
			return this.viewProxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(printWriter, context);

		srcWriter.indent();
		this.generateProxy(logger, srcWriter);
		srcWriter.println();
		this.generateTokenPrefixes(logger, srcWriter);
		srcWriter.println();
		if (placeTokenizerClass == null) {
			generateInternalTokenizer(logger, srcWriter);
		}
		else {
			generateDelegateTokenizer(logger, srcWriter);
		}
		generateActivityFactory(logger, srcWriter);
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
		for (JConstructor constructor : placeType.getConstructors()) {
			if (constructor.getParameters().length == 1
					&& constructor.getParameters()[0].getType().getSimpleSourceName().equals(String.class.getSimpleName())) {
				hasTokeConstructor = true;
			}
		}
		srcWriter.println("@Override");
		srcWriter.println("public %s getPlace(String token) {", placeType.getSimpleSourceName());
		srcWriter.indent();
		if (hasTokeConstructor) {
			srcWriter.println("return new %s(token);", placeType.getSimpleSourceName());
		}
		else {
			srcWriter.println("return new %s();", placeType.getSimpleSourceName());
		}
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("@Override");
		srcWriter.println("public String getToken(%s place) {", placeType.getSimpleSourceName());
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
		srcWriter.println("public %s getPlace(String token) {", placeType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("return new %s().getPlace(token);", placeTokenizerClass.getSimpleName());
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("@Override");
		srcWriter.println("public String getToken(%s place) {", placeType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("return new %s().getToken(place);", placeTokenizerClass.getSimpleName());
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateTokenPrefixes(TreeLogger logger, SourceWriter srcWriter) {
		List<String> tokens = Lists.newArrayList();
		tokens.add("\"" + this.placeType.getSimpleSourceName().replaceAll("Place$", "") + "\"");
		for (String alias : activityDescrition.aliases()) {
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

		String viewName = activityDescrition.view().getSimpleName();
		srcWriter.println();
		srcWriter.println("private static %s view;", viewName);
		srcWriter.println();
		srcWriter.println("@Override");
		srcWriter.println("public void getView(final ViewProxy.Callback callback) {");
		srcWriter.indent();
		if (activityDescrition.asyncView()) {
			srcWriter.println("GWT.runAsync(%s.class, new RunAsyncCallback() {", viewName);
			srcWriter.indent();
			srcWriter.println("public void onFailure(Throwable reason) {");
			srcWriter.indent();
			srcWriter.println("throw new RuntimeException(reason);");
			srcWriter.outdent();
			srcWriter.println("}");
			srcWriter.println("public void onSuccess() {");
			srcWriter.indent();
			srcWriter.println("if(view == null || %s){", activityDescrition.scope() == Scope.PROTOTYPE);
			srcWriter.indent();
			srcWriter.println("view = GWT.create(%s.class);", viewName);
			srcWriter.outdent();
			srcWriter.println("}");
			generateProxyResult(logger, srcWriter);
			// srcWriter.println("callback.showView(view);");
			srcWriter.outdent();
			srcWriter.println("}");
			srcWriter.outdent();
			srcWriter.println("});");
		}
		else {
			srcWriter.println("if(view == null || %s){", activityDescrition.scope() == Scope.PROTOTYPE);
			srcWriter.indent();
			srcWriter.println("view = GWT.create(%s.class);", viewName);
			srcWriter.outdent();
			srcWriter.println("}");
			generateProxyResult(logger, srcWriter);
			// srcWriter.println("callback.showView(view);");

		}
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private void generateProxyResult(TreeLogger logger, SourceWriter srcWriter) {
		if (viewDecoratorClass == null) {
			srcWriter.println("callback.showView(view);");
		}
		else {
			String decoratorName = viewDecoratorClass.getSimpleName();
			srcWriter.println("%s decorator = %s.get();", decoratorName, decoratorName);
			srcWriter.println("decorator.setWidget(view);");
			srcWriter.println("decorator.presentPlace(%s.this);", viewProxySimpleName);
			srcWriter.println("callback.showView(decorator);");
		}
	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {
		ClassSourceFileComposerFactory composerFactory = new ClassSourceFileComposerFactory(packageName, viewProxySimpleName);

		composerFactory.setSuperclass(placeType.getSimpleSourceName());

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(RunAsyncCallback.class.getName());
		composerFactory.addImport(ViewProxy.class.getName());
		composerFactory.addImport(Place.class.getName());
		composerFactory.addImport(ViewPlace.class.getName());
		composerFactory.addImport(Activity.class.getName());
		composerFactory.addImport(ViewActivity.class.getName());
		composerFactory.addImport(placeType.getQualifiedSourceName());
		composerFactory.addImport(activityDescrition.view().getCanonicalName());
		if (placeTokenizerClass != null) {
			composerFactory.addImport(placeTokenizerClass.getCanonicalName());
		}
		if (viewDecoratorClass != null) {
			composerFactory.addImport(viewDecoratorClass.getCanonicalName());
		}

		composerFactory.addImplementedInterface(ViewProxy.class.getSimpleName() + "<" + placeType.getSimpleSourceName() + ">");

		return composerFactory.createSourceWriter(ctx, printWriter);
	}
}
