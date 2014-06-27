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
package fr.putnami.pwt.core.widget.client.base;

import java.util.Collection;
import java.util.List;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.gwt.event.logical.shared.HasValueChangeHandlers;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.model.client.base.HasMessageHelper;
import fr.putnami.pwt.core.widget.client.event.AskFocusEvent;
import fr.putnami.pwt.core.widget.client.event.ChangeEvent;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractInputChoice<T, U> extends AbstractInput<U> implements
		HasValueChangeHandlers<U>,
		ValueChangeHandler<U>,
		HasMessageHelper {

	public interface ChoiceSelectionHandler<T, U> {

		void onItemClick(T item);

		void setSelection(U selection, boolean fireEvents);

		U getSelection();
	}

	public interface ChoiceRenderer<T> {

		Renderer<T> getItemRenderer();

		void setItemRenderer(Renderer<T> renderer);

		String renderItem(T item);

		String getNullRender();

		void setNullRender(String nullRender);

	}

	public class ChoiceRendererImpl implements ChoiceRenderer<T> {

		public static final String DEFAULT_NULL_RENDER = "";

		private Renderer<T> itemRenderer;
		private String nullRender;

		@Override
		public Renderer<T> getItemRenderer() {
			return itemRenderer;
		}

		@Override
		public void setItemRenderer(Renderer<T> renderer) {
			this.itemRenderer = renderer;
		}

		@Override
		public String renderItem(T item) {
			return render(item, itemRenderer);
		}

		@Override
		public String getNullRender() {
			return nullRender;
		}

		@Override
		public void setNullRender(String nullRender) {
			this.nullRender = nullRender;
		}

		protected <V> String render(V value, Renderer<V> renderer) {
			if (value == null) {
				return renderNull();
			}
			if (renderer != null) {
				return renderer.render(value);
			}
			return value.toString();
		}

		protected String renderNull() {
			if (AbstractInputChoice.this.nullValueAllowed) {
				return this.nullRender != null ? this.nullRender : DEFAULT_NULL_RENDER;
			}
			return DEFAULT_NULL_RENDER;
		}
	}

	private HandlerRegistration valueChangeRegistration;
	private HandlerRegistration askFocusRegistration;

	private boolean nullValueAllowed = false;
	private boolean multiple = false;
	private List<T> orderedItems = Lists.newArrayList();
	private String htmlId;

	private MessageHelper messageHelper;

	public AbstractInputChoice(Widget content) {
		super(content);
	}

	protected AbstractInputChoice(Widget content, AbstractInputChoice<T, U> source) {
		super(content, source);
		messageHelper = source.messageHelper;
		multiple = source.multiple;
		nullValueAllowed = source.nullValueAllowed;
		orderedItems = source.orderedItems;

		setSelectionHandler(source.getSelectionHandler());
		setChoiceRenderer(source.getChoiceRenderer());
	}

	@Override
	protected void endConstruct() {
		StyleUtils.removeStyle(this, STYLE_CONTROL);
		addValueChangeHandler(this);
		super.endConstruct();
		redrawInternal();
	}

	public void setItems(Collection<T> items) {
		orderedItems.clear();
		orderedItems.addAll(items);
	}

	public void setNullRender(String nullRender) {
		getChoiceRenderer().setNullRender(nullRender);
	}

	public void setItemRenderer(Renderer<T> renderer) {
		getChoiceRenderer().setItemRenderer(renderer);
	}

	public void setNullValueAllowed(boolean nullValueAllowed) {
		this.nullValueAllowed = nullValueAllowed;
	}

	public boolean isNullValueAllowed() {
		return nullValueAllowed;
	}

	public void setMultiple(boolean multiple) {
		this.multiple = multiple;
	}

	public boolean isMultiple() {
		return multiple;
	}

	@Override
	public void setHtmlId(String htmlId) {
		this.htmlId = htmlId;

		if (askFocusRegistration != null) {
			askFocusRegistration.removeHandler();
		}
		if (htmlId != null) {
			AskFocusEvent.Handler handler = new AskFocusEvent.Handler() {

				@Override
				public void onAskFocus(AskFocusEvent event) {
					if (Objects.equal(event.getHtmlId(), AbstractInputChoice.this.htmlId)) {
						setFocus(true);
					}
				}
			};
			askFocusRegistration = EventBus.get().addHandler(AskFocusEvent.TYPE, handler);
		}
	}

	@Override
	public String getHtmlId() {
		return htmlId;
	}

	@Override
	public MessageHelper getMessageHelper() {
		return messageHelper;
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
	}

	@Override
	public boolean isDirty() {
		return !Objects.equal(this.getValue(), this.getSelectedValue());
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		if (valueChangeRegistration == null) {
			valueChangeRegistration = addValueChangeHandler(new ChangeEvent<U>(AbstractInputChoice.this));
		}
		return super.addDirtyHandler(handler);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addValueChangeHandler(ValueChangeHandler<U> handler) {
		return addHandler(handler, ValueChangeEvent.getType());
	}

	public U getSelectedValue() {
		return getSelectionHandler().getSelection();
	}

	@Override
	public U flush() {
		clearErrors();
		U value = getSelectedValue();
		validate(value);
		if (!hasErrors()) {
			setValue(value);
		}
		return getValue();
	}

	@Override
	public void edit(U value) {
		redrawInternal();
		setValue(value);
		getSelectionHandler().setSelection(getValue(), false);
	}

	@Override
	public void onValueChange(ValueChangeEvent<U> event) {
		// Do Nothing
	}

	protected abstract void redrawInternal();

	protected abstract ChoiceRenderer<T> getChoiceRenderer();

	protected abstract void setChoiceRenderer(ChoiceRenderer<T> choiceRenderer);

	protected abstract ChoiceSelectionHandler<T, U> getSelectionHandler();

	protected abstract void setSelectionHandler(ChoiceSelectionHandler<T, U> selectionHandler);

	protected List<T> getOrderedItems() {
		return orderedItems;
	}

}
