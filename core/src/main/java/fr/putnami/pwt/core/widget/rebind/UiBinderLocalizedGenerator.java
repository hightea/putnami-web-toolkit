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
package fr.putnami.pwt.core.widget.rebind;

import com.google.gwt.core.ext.Generator;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.PropertyOracle;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.TypeOracle;
import com.google.gwt.i18n.rebind.LocaleUtils;
import com.google.gwt.i18n.shared.GwtLocale;

public class UiBinderLocalizedGenerator extends Generator {

	@Override
	public String generate(TreeLogger logger, GeneratorContext context, String typeName)
		throws UnableToCompleteException {
		TypeOracle typeOracle = context.getTypeOracle();
		assert (typeOracle != null);

		JClassType viewType = typeOracle.findType(typeName);
		if (viewType == null) {
			logger.log(TreeLogger.ERROR, "Unable to find metadata for type '" + typeName + "'", null);
			throw new UnableToCompleteException();
		}

		PropertyOracle propertyOracle = context.getPropertyOracle();
		LocaleUtils localeUtils = LocaleUtils.getInstance(logger, propertyOracle, context);
		GwtLocale locale = localeUtils.getCompileLocale();

		UiBinderLocalizedCreator creator = new UiBinderLocalizedCreator(viewType, locale);
		String proxyClassName = creator.create(logger, context);
		return proxyClassName;
	}

}
