package fr.putnami.pwt.core.inject.rebind;

import java.util.Collection;

import com.google.gwt.core.ext.typeinfo.JClassType;

public interface InjectorDelegateFactorty {

	Collection<InjectorCreatorDelegate> createDelegates(JClassType injectableType);
}
