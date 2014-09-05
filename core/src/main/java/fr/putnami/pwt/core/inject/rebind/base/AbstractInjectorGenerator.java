package fr.putnami.pwt.core.inject.rebind.base;

import com.google.gwt.core.ext.Generator;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.TypeOracle;

public abstract class AbstractInjectorGenerator<C extends AbstractInjectorCreator> extends Generator {

	@Override
	public String generate(TreeLogger logger, GeneratorContext context, String typeName) throws UnableToCompleteException {
		TypeOracle typeOracle = context.getTypeOracle();
		assert typeOracle != null;

		JClassType viewType = typeOracle.findType(typeName);
		if (viewType == null) {
			logger.log(TreeLogger.ERROR, "Unable to find metadata for type '" + typeName + "'", null);
			throw new UnableToCompleteException();
		}

		C injectorCreator = newCreator(viewType);
		if (injectorCreator.shallRebind()) {
			return injectorCreator.create(logger, context);
		}
		return typeName;
	}

	protected abstract C newCreator(JClassType viewType);
}
