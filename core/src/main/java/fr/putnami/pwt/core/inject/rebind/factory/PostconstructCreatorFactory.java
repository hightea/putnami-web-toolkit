package fr.putnami.pwt.core.inject.rebind.factory;

import java.util.Collection;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JMethod;

import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectPostconstructCreator;
import fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil;

public class PostconstructCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public Collection<InjectorCreatorDelegate> createDelegates(JClassType injectableType) {
		Collection<InjectorCreatorDelegate> delegates = Lists.newArrayList();
		Collection<JMethod> methods = InjectCreatorUtil.listMethod(injectableType, PostConstruct.class);
		for (JMethod postConstructMethod : methods) {
			delegates.add(new InjectPostconstructCreator(postConstructMethod));
		}
		return delegates;
	}

}
