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
package fr.putnami.pwt.core.editor.client.helper;

import com.google.common.base.CaseFormat;
import com.google.common.collect.Maps;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.regexp.shared.RegExp;

import java.util.Map;
import java.util.MissingResourceException;

import fr.putnami.pwt.core.editor.client.Path;

public class MessageHelper {

	private final Map<Class<?>, ConstantsWithLookup> constantRegistry = Maps.newHashMap();

	public MessageHelper(ConstantsWithLookup defaultConstants) {
		if (defaultConstants != null) {
			this.registerConstants(defaultConstants, Object.class);
		}
	}

	public void registerConstants(ConstantsWithLookup constants, Class<?> propertyType) {
		if (constants != null) {
			this.constantRegistry.put(propertyType == null ? Object.class : propertyType, constants);
		}
	}

	public String getMessage(Class<?> beanType, Path path, String suffix) {
		String label = null;
		int i = 0;
		while (i < path.size() && label == null) {
			Path keyPath = path.subPath(i);
			String key = this.getMessageKey(keyPath, suffix);
			label = this.findMessage(beanType, key);
			if (label != null) {
				return label;
			}
			i++;
		}
		return null;
	}

	public String createDefaultMessage(String key) {
		if (key == null) {
			return null;
		}
		return "[" + key + "]";
	}

	public String findMessage(Class<?> propertyType, String key) {
		if (key == null) {
			return null;
		}
		Class<?> typeToLookup = propertyType == null ? Object.class : propertyType;
		String label = null;
		while (typeToLookup != null && label == null) {
			try {
				ConstantsWithLookup constants = this.constantRegistry.get(typeToLookup);
				if (constants != null) {
					label = constants.getString(key);
				}
			} catch (MissingResourceException exc) {
				// Do Nothing
			}
			typeToLookup = typeToLookup.getSuperclass();
		}

		return label;
	}

	public String getMessageKey(Path path, String suffix) {
		if (path == null || path.isEmpty()) {
			return null;
		}

		String labelKey = path.toString().replaceAll("\\.", "_").replaceAll("\\[", "").replaceAll("]", "").toUpperCase();

		labelKey = CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, String.valueOf(labelKey));
		if (suffix != null) {
			labelKey += suffix;
		}
		return labelKey;
	}

	public static String replaceParams(String pMessage, Object... parameters) {
		int i = 0;
		String message = pMessage;
		if (message != null && parameters != null && parameters.length > 0) {
			for (Object param : parameters) {
				RegExp pattern = RegExp.compile("\\{" + i + "\\}");
				String stringParam = param + "";
				message = pattern.replace(message, stringParam);
				i++;
			}
		}
		return message;
	}

}
