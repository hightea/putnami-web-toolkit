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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.base.HasMessageHelper;
import fr.putnami.pwt.core.widget.client.base.AbstractOutput;
import fr.putnami.pwt.core.widget.client.helper.EnumRenderer;

public class OutputEnum<E extends Enum<E>> extends AbstractOutput<E> implements HasMessageHelper {

	private MessageHelper messageHelper;

	public OutputEnum() {
		setRenderer(new EnumRenderer<E>(messageHelper));
	}

	protected OutputEnum(OutputEnum<E> source) {
		super(source);
		messageHelper = source.messageHelper;
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputEnum<E>(this);
	}

	@Override
	public MessageHelper getMessageHelper() {
		return messageHelper;
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
		setRenderer(new EnumRenderer<E>(messageHelper));
	}
}
