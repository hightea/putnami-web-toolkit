package fr.putnami.pwt.core.inject.rebind.factory;

import java.util.Collection;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JMethod;

import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectPresenterCreator;
import fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil;

public class PresenterCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public Collection<InjectorCreatorDelegate> createDelegates(JClassType injectableType) {
		Collection<InjectorCreatorDelegate> delegates = Lists.newArrayList();
		Collection<JMethod> methods = InjectCreatorUtil.listMethod(injectableType, PresentHandler.class);
		delegates.add(new InjectPresenterCreator(methods));
		return delegates;
	}

}
