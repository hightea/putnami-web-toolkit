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
package fr.putnami.pwt.core.inject.rebind.base;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.io.PrintWriter;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public abstract class AbstractInjectorCreator {
	public static final String PROXY_SUFFIX = "_Injector";

	protected final JClassType injectableType;
	protected final String packageName;
	protected final String proxyName;
	protected final String proxyQualifiedName;

	protected final List<InjectorCreatorDelegate> delegates = Lists.newArrayList();

	public AbstractInjectorCreator(JClassType injectableType) {
		this.injectableType = injectableType;
		this.packageName = injectableType.getPackage().getName();
		this.proxyName = injectableType.getSimpleSourceName() + AbstractInjectorCreator.PROXY_SUFFIX;
		this.proxyQualifiedName = injectableType.getPackage().getName() + "." + this.proxyName;
	}

	public boolean shallRebind() {
		if (this.delegates.isEmpty()) {
			for (InjectorDelegateFactorty factory : this.getFactories()) {
				factory.createDelegates(this.injectableType, this.delegates);
			}

			Collections.sort(this.delegates, new Comparator<InjectorCreatorDelegate>() {
				@Override
				public int compare(InjectorCreatorDelegate o1, InjectorCreatorDelegate o2) {
					return Integer.compare(o1.getOrder(), o2.getOrder());
				}
			});
		}
		return !this.delegates.isEmpty();
	}

	protected abstract Collection<InjectorDelegateFactorty> getFactories();

	protected void doCreate(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		this.doSubGeneration(logger, context);
		this.writeStatics(logger, context, srcWriter);
		this.writeConstructor(logger, context, srcWriter);
		this.writeMethods(logger, context, srcWriter);
	}

	public String create(TreeLogger logger, GeneratorContext context) {
		PrintWriter writer = context.tryCreate(logger, this.packageName, this.proxyName);
		if (writer == null) {
			return this.proxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(writer, context);
		srcWriter.indent();

		this.doCreate(logger, context, srcWriter);

		srcWriter.outdent();
		srcWriter.commit(logger);

		return this.proxyQualifiedName;
	}

	// methods
	protected void writeMethods(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		for (InjectorWritterMethod delegate : Iterables.filter(this.delegates, InjectorWritterMethod.class)) {
			delegate.writeMethods(srcWriter);
		}
	}

	// constructor
	protected void writeConstructor(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		srcWriter.println("public %s() {", this.proxyName);
		srcWriter.indent();
		for (InjectorWritterConstructor delegate : Iterables.filter(this.delegates, InjectorWritterConstructor.class)) {
			delegate.writeConstructor(srcWriter);
		}
		srcWriter.outdent();
		srcWriter.println("}");
	}

	// inner class
	protected void writeStatics(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		for (InjectorWritterStatic delegate : Iterables.filter(this.delegates, InjectorWritterStatic.class)) {
			delegate.writeStatic(srcWriter);
		}
	}

	// Sub generations
	protected void doSubGeneration(TreeLogger logger, GeneratorContext context) {
		for (InjectorWritterSubGenerate delegate : Iterables.filter(this.delegates, InjectorWritterSubGenerate.class)) {
			delegate.subGenerate(logger, context);
		}
	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {
		ClassSourceFileComposerFactory composerFactory =
			new ClassSourceFileComposerFactory(this.packageName, this.proxyName);

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(this.injectableType.getQualifiedSourceName());

		composerFactory.setSuperclass(this.injectableType.getSimpleSourceName());

		for (InjectorWritterInit delegate : Iterables.filter(this.delegates, InjectorWritterInit.class)) {
			delegate.initComposer(composerFactory);
		}

		return composerFactory.createSourceWriter(ctx, printWriter);
	}

}
