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

import java.util.Map;

import com.google.common.collect.Maps;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.InputElement;
import com.google.gwt.dom.client.LabelElement;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractInputChoice;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class InputRadio<T> extends AbstractInputChoice<T, T> {

	public enum Type implements CssStyle {
		DEFAULT("radio"),
		INLINE("radio-inline");

		private final String style;

		private Type(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	private static long seq = 0;

	static long incrementeAndGetSeq() {
		return ++seq;
	}

	private class SingleChoiceHandler implements ChoiceSelectionHandler<T, T> {
		private T selectedItem;

		@Override
		public void onItemClick(T item) {
			selectedItem = item;
			ValueChangeEvent.fire(InputRadio.this, selectedItem);
		}

		@Override
		public void setSelection(T selection, boolean fireEvents) {
			T oldValue = selectedItem;
			RadioContainer newSelection = InputRadio.this.itemsContainer.get(selection);
			if (newSelection != null) {
				newSelection.select();
			}
			selectedItem = selection;
			if (fireEvents) {
				ValueChangeEvent.fireIfNotEqual(InputRadio.this, oldValue, selection);
			}
		}

		@Override
		public T getSelection() {
			return selectedItem;
		}
	}

	private class RadioContainer extends Composite implements ClickHandler {

		private final InputElement radioElement = InputElement.as(Document.get().createRadioInputElement("radio" + InputRadio.this.radioGroupNum));

		private T value;

		public RadioContainer(T value) {
			this.value = value;
			Container container = null;
			Container label = new Container(LabelElement.TAG);
			switch (InputRadio.this.type) {
			case INLINE:
				container = label;
				break;
			default:
				container = new Container();
				container.append(label);
				break;
			}
			initWidget(container);
			label.getElement().appendChild(radioElement);
			label.getElement().appendChild(Document.get().createTextNode(" " + getChoiceRenderer().renderItem(value)));
			StyleUtils.addStyle(container, InputRadio.this.type);
			container.addDomHandler(this, ClickEvent.getType());
			InputRadio.this.itemsContainer.put(value, this);
		}

		@Override
		public void onClick(ClickEvent event) {
			InputRadio.this.getSelectionHandler().onItemClick(value);
		}

		public void select() {
			radioElement.setChecked(true);
		}

	}

	private final long radioGroupNum = incrementeAndGetSeq();

	private ChoiceSelectionHandler<T, T> selectionHandler = new SingleChoiceHandler();
	private ChoiceRenderer<T> choiceRenderer = new ChoiceRendererImpl();

	private final Container container;
	private final Map<T, RadioContainer> itemsContainer = Maps.newHashMap();
	private Type type = Type.DEFAULT;

	public InputRadio() {
		super(new Container());
		container = (Container) getWidget();
		setMultiple(true);
		endConstruct();
	}

	protected InputRadio(InputRadio<T> source) {
		super(new Container(), source);
		container = (Container) getWidget();
		endConstruct();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputRadio<T>(this);
	}

	@Override
	protected void redrawInternal() {
		container.clear();
		itemsContainer.clear();
		for (T item : getOrderedItems()) {
			container.add(new RadioContainer(item));
		}
	}

	public void setType(Type type) {
		this.type = type;
	}

	@Override
	protected ChoiceRenderer<T> getChoiceRenderer() {
		return choiceRenderer;
	}

	@Override
	protected void setChoiceRenderer(ChoiceRenderer<T> choiceRenderer) {
		this.choiceRenderer = choiceRenderer;
	}

	@Override
	protected ChoiceSelectionHandler<T, T> getSelectionHandler() {
		return selectionHandler;
	}

	@Override
	protected void setSelectionHandler(ChoiceSelectionHandler<T, T> selectionHandler) {
		this.selectionHandler = selectionHandler;
	}

}
