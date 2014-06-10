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
package fr.putnami.pwt.core.editor.client;

import com.google.gwt.user.client.ui.IsWidget;

public interface EditorLabel extends Editor, IsWidget {

	String EMPTY_SUFFIX = "";
	String LABEL_SUFFIX = "Label";
	String TOOLTIP_SUFFIX = "Tooltip";
	String HEADER_SUFFIX = "Header";
	String BUTTON_SUFFIX = "Button";
	String HELP_SUFFIX = "Help";
	String CHECKBOX_SUFFIX = "Checkbox";

	String getLabelKey();

	boolean isLabelMandatory();

	String[] getSuffix();

	String getText();

	void setText(String text);

}
