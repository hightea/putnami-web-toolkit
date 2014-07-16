package fr.putnami.pwt.core.inject.rebind.factory;

import java.util.Collection;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectTemplateCreator;

public class TemplatedCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public Collection<InjectorCreatorDelegate> createDelegates(JClassType injectableType) {
		Collection<InjectorCreatorDelegate> delegates = Lists.newArrayList();
		if (injectableType.getAnnotation(Templated.class) != null) {
			delegates.add(new InjectTemplateCreator(injectableType));
		}
		return delegates;
	}

}
