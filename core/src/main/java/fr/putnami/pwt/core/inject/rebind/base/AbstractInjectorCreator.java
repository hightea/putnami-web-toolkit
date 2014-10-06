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
		this.proxyName = injectableType.getSimpleSourceName() + PROXY_SUFFIX;
		this.proxyQualifiedName = injectableType.getPackage().getName() + "." + proxyName;

	}

	public boolean shallRebind() {
		if (delegates.isEmpty()) {
			for (InjectorDelegateFactorty factory : getFactories()) {
				factory.createDelegates(injectableType, this.delegates);
			}

			Collections.sort(this.delegates, new Comparator<InjectorCreatorDelegate>() {
				@Override
				public int compare(InjectorCreatorDelegate o1, InjectorCreatorDelegate o2) {
					return Integer.compare(o1.getOrder(), o2.getOrder());
				}
			});
		}
		return delegates.size() > 0;
	}

	protected abstract Collection<InjectorDelegateFactorty> getFactories();

	protected void doCreate(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		doSubGeneration(logger, context);
		writeStatics(logger, context, srcWriter);
		writeConstructor(logger, context, srcWriter);
		writeMethods(logger, context, srcWriter);
	}

	public String create(TreeLogger logger, GeneratorContext context) {
		PrintWriter writer = context.tryCreate(logger, packageName, proxyName);
		if (writer == null) {
			return this.proxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(writer, context);
		srcWriter.indent();

		doCreate(logger, context, srcWriter);

		srcWriter.outdent();
		srcWriter.commit(logger);

		return this.proxyQualifiedName;

	}


	// methods
	protected void writeMethods(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		for (InjectorWritterMethod delegate : Iterables.filter(delegates, InjectorWritterMethod.class)) {
			delegate.writeMethods(srcWriter);
		}
	}

	// constructor
	protected void writeConstructor(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		srcWriter.println("public %s() {", proxyName);
		srcWriter.indent();
		for (InjectorWritterConstructor delegate : Iterables.filter(delegates, InjectorWritterConstructor.class)) {
			delegate.writeConstructor(srcWriter);
		}
		srcWriter.outdent();
		srcWriter.println("}");
	}

	// inner class
	protected void writeStatics(TreeLogger logger, GeneratorContext context, SourceWriter srcWriter) {
		for (InjectorWritterStatic delegate : Iterables.filter(delegates, InjectorWritterStatic.class)) {
			delegate.writeStatic(srcWriter);
		}
	}

	// Sub generations
	protected void doSubGeneration(TreeLogger logger, GeneratorContext context) {
		for (InjectorWritterSubGenerate delegate : Iterables.filter(delegates, InjectorWritterSubGenerate.class)) {
			delegate.subGenerate(logger, context);
		}
	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {
		ClassSourceFileComposerFactory composerFactory = new ClassSourceFileComposerFactory(packageName, proxyName);

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(this.injectableType.getQualifiedSourceName());

		composerFactory.setSuperclass(this.injectableType.getSimpleSourceName());

		for (InjectorWritterInit delegate : Iterables.filter(delegates, InjectorWritterInit.class)) {
			delegate.initComposer(composerFactory);
		}

		return composerFactory.createSourceWriter(ctx, printWriter);
	}

}
