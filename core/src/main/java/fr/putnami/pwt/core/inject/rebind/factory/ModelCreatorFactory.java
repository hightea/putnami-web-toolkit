package fr.putnami.pwt.core.inject.rebind.factory;

import java.util.Collection;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;

import fr.putnami.pwt.core.inject.client.annotation.InjectModel;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectModelCreator;
import fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil;

public class ModelCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public Collection<InjectorCreatorDelegate> createDelegates(JClassType injectableType) {
		Collection<InjectorCreatorDelegate> delegates = Lists.newArrayList();
		Collection<JField> fields = InjectCreatorUtil.listFields(injectableType, InjectModel.class);
		for (JField field : fields) {
			delegates.add(new InjectModelCreator(field));
		}
		return delegates;
	}

}
