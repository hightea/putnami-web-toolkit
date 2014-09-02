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
package fr.putnami.pwt.core.inject.rebind;

import java.io.PrintWriter;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.factory.DecoratorPresenterCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.InitializeFormCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.MayStopActivityCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.ModelCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.PostconstructCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.PresenterCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.ResourceCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.SecurityCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.ServiceCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.StopActivityCreatorFactory;
import fr.putnami.pwt.core.inject.rebind.factory.TemplatedCreatorFactory;

public class InjectorProxyCreator {
	public static final String PROXY_SUFFIX = "_Injector";

	private static final Collection<InjectorDelegateFactorty> delegateFactories = Lists.newArrayList();

	static {
		delegateFactories.add(new ResourceCreatorFactory());
		delegateFactories.add(new ServiceCreatorFactory());
		delegateFactories.add(new PresenterCreatorFactory());
		delegateFactories.add(new MayStopActivityCreatorFactory());
		delegateFactories.add(new StopActivityCreatorFactory());
		delegateFactories.add(new DecoratorPresenterCreatorFactory());
		delegateFactories.add(new ModelCreatorFactory());
		delegateFactories.add(new TemplatedCreatorFactory());
		delegateFactories.add(new PostconstructCreatorFactory());
		delegateFactories.add(new SecurityCreatorFactory());
		delegateFactories.add(new InitializeFormCreatorFactory());
	}
	private final JClassType injectableType;
	private final String packageName;
	private final String proxyName;
	private final String proxyQualifiedName;

	private final List<InjectorCreatorDelegate> delegates = Lists.newArrayList();

	public InjectorProxyCreator(JClassType injectableType) {
		this.injectableType = injectableType;
		this.packageName = injectableType.getPackage().getName();
		this.proxyName = injectableType.getSimpleSourceName() + PROXY_SUFFIX;
		this.proxyQualifiedName = injectableType.getPackage().getName() + "." + proxyName;

		for (InjectorDelegateFactorty factory : delegateFactories) {
			factory.createDelegates(injectableType, this.delegates);
		}

		Collections.sort(this.delegates, new Comparator<InjectorCreatorDelegate>() {
			@Override
			public int compare(InjectorCreatorDelegate o1, InjectorCreatorDelegate o2) {
				return Integer.compare(o1.getOrder(), o2.getOrder());
			}
		});
	}

	public boolean shallRebind(){
		return delegates.size() > 0;
	}

	public String create(TreeLogger logger, GeneratorContext context) {
		PrintWriter writer = context.tryCreate(logger, packageName, proxyName);
		if (writer == null) {
			return this.proxyQualifiedName;
		}

		SourceWriter srcWriter = this.getSourceWriter(writer, context);
		srcWriter.indent();

		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.create(logger, context);
		}

		// inner class
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeStatic(srcWriter);
		}
		// fields
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeFields(srcWriter);
		}
		// constructor
		srcWriter.println("public %s() {", proxyName);
		srcWriter.indent();
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeConstructor(srcWriter);
		}
		srcWriter.outdent();
		srcWriter.println("}");
		// presenter
		srcWriter.println("public <P extends Place> void present(P place, final AcceptsOneWidget displayer){");
		srcWriter.indent();
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeBeforePresent(srcWriter);
		}
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writePresent(srcWriter);
		}
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeAfterPresent(srcWriter);
		}
		srcWriter.outdent();
		srcWriter.println("}");

		// methods
		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.writeMethods(srcWriter);
		}
		srcWriter.outdent();
		srcWriter.commit(logger);

		return this.proxyQualifiedName;

	}

	private SourceWriter getSourceWriter(PrintWriter printWriter, GeneratorContext ctx) {
		ClassSourceFileComposerFactory composerFactory = new ClassSourceFileComposerFactory(packageName, proxyName);

		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(AcceptsOneWidget.class.getName());

		composerFactory.addImport(this.injectableType.getQualifiedSourceName());

		composerFactory.setSuperclass(this.injectableType.getSimpleSourceName());

		for (InjectorCreatorDelegate delegate : delegates) {
			delegate.initComposer(composerFactory);
		}
		return composerFactory.createSourceWriter(ctx, printWriter);
	}

}
