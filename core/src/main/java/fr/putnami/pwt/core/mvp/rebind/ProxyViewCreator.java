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
import java.util.logging.Logger;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.RunAsyncCallback;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.NotFoundException;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.mvp.client.ViewProxy;
import fr.putnami.pwt.core.mvp.client.exception.ApplicationUnreachableException;

public class ProxyViewCreator {

	private static final String PROXY_SUFFIX = "_ProxyView";

	private final JClassType viewType;

	private String viewProxyQualifiedName;
	private String viewProxySimpleName;

	public ProxyViewCreator(JClassType viewType) {
		this.viewType = viewType;
		this.viewProxyQualifiedName = this.viewType.getQualifiedSourceName() + ProxyViewCreator.PROXY_SUFFIX;
		this.viewProxySimpleName = this.viewType.getSimpleSourceName() + ProxyViewCreator.PROXY_SUFFIX;
	}

	public String create(TreeLogger logger, GeneratorContext context) throws UnableToCompleteException, NotFoundException {
		PrintWriter printWriter = this.getPrintWriter(logger, context, this.viewProxyQualifiedName);
		if (printWriter == null) {
			return this.viewProxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(printWriter, context);

		srcWriter.println();
		srcWriter.indent();
		this.generateProxy(logger, srcWriter);
		srcWriter.println();
		srcWriter.outdent();
		srcWriter.commit(logger);

		return this.viewProxyQualifiedName;
	}

	private void generateProxy(TreeLogger logger, SourceWriter srcWriter) {
		srcWriter.println();
		srcWriter.println("private static %s view;", this.viewType.getSimpleSourceName());
		srcWriter.println();

		srcWriter.println("public void getView(final ViewProxy.Callback callback) {");
		srcWriter.indent();
		srcWriter.println("GWT.runAsync(%s.class, new RunAsyncCallback() {", this.viewType.getSimpleSourceName());
		srcWriter.indent();
		srcWriter.println("public void onFailure(Throwable reason) {");
		srcWriter.indent();
		srcWriter.println("if (ApplicationUnreachableException.HTTP_DOWNLOAD_FAILURE_EXCEPTION.equals(reason.getClass().getSimpleName())) {");
		srcWriter.indent();
		srcWriter.println("reason = new ApplicationUnreachableException(reason);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("GWT.reportUncaughtException(reason);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("public void onSuccess() {");
		srcWriter.indent();
		srcWriter.println("if(view == null){");
		srcWriter.indent();
		srcWriter.println("view = new %s();", this.viewType.getSimpleSourceName());
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("callback.showView(view);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.outdent();
		srcWriter.println("});");
		srcWriter.outdent();
		srcWriter.println("}");
	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {

		String packageName = this.viewType.getPackage().getName();
		String className = this.viewProxySimpleName;

		ClassSourceFileComposerFactory composerFactory = new ClassSourceFileComposerFactory(packageName, className);

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(RunAsyncCallback.class.getName());

		composerFactory.addImport(ViewProxy.class.getName());
		composerFactory.addImport(Logger.class.getName());
		composerFactory.addImport(ApplicationUnreachableException.class.getName());

		composerFactory.addImport(this.viewType.getQualifiedSourceName());

		composerFactory.addImplementedInterface(ViewProxy.class.getSimpleName());

		return composerFactory.createSourceWriter(ctx, printWriter);
	}

	private PrintWriter getPrintWriter(TreeLogger logger, GeneratorContext ctx, String targetQualifiedName) {
		String packageName = this.viewType.getPackage().getName();
		String className = this.viewProxySimpleName;
		return ctx.tryCreate(logger, packageName, className);
	}

}
