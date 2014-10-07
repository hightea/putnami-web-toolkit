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
package fr.putnami.pwt.core.widget.client.helper;

import com.google.common.base.CaseFormat;
import com.google.gwt.text.shared.AbstractRenderer;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;

public final class EnumRenderer<E extends Enum<E>> extends AbstractRenderer<E> {

	public static final String ENUM_SUFFIX = "Enum";

	private final MessageHelper messageHelper;

	public EnumRenderer(MessageHelper messageHelper) {
		super();
		this.messageHelper = messageHelper;
	}

	public String getEnumKey(E value) {
		if (value == null) {
			return null;
		}
		StringBuilder result = new StringBuilder();
		result
			.append(CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, this.getSimpleClassName(value)));
		result.append(CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, value.toString()));
		result.append(EnumRenderer.ENUM_SUFFIX);
		return result.toString();
	}

	private String getSimpleClassName(E value) {
		String result = value.getClass().getSimpleName();
		if (result.indexOf("$") != -1) {
			// In case of inner class
			result = result.substring(result.indexOf("$") + 1);
		}
		return result;
	}

	@Override
	public String render(E object) {
		if (object == null) {
			return "";
		}

		if (this.messageHelper != null) {
			String result = this.messageHelper.findMessage(object.getClass(), this.getEnumKey(object));
			if (result != null) {
				return result;
			}
		}

		return object.toString();
	}
}
