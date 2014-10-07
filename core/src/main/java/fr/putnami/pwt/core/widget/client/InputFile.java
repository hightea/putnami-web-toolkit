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
package fr.putnami.pwt.core.widget.client;

import com.google.common.base.Strings;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.dom.client.DataTransfer;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.InputElement;
import com.google.gwt.dom.client.Style.Overflow;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.DragLeaveEvent;
import com.google.gwt.event.dom.client.DragLeaveHandler;
import com.google.gwt.event.dom.client.DragOverEvent;
import com.google.gwt.event.dom.client.DragOverHandler;
import com.google.gwt.event.dom.client.DropEvent;
import com.google.gwt.event.dom.client.DropHandler;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteHandler;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.event.client.HandlerRegistrationCollection;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.Button.Type;
import fr.putnami.pwt.core.widget.client.base.AbstractInput;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.constant.WidgetParams;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.UUID;
import fr.putnami.pwt.core.widget.shared.domain.FileDto;
import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;

public class InputFile extends InputGroup<FileDto> implements HasDrawable {

	private static final String MULTIPART_BOUNDARY = "x-x-x-x-x";
	private static final String EOL = "\r\n";

	private static final String URL_UPLOAD = GWT.getHostPageBaseURL() + "file/upload/";
	private static final String URL_STATUS = GWT.getHostPageBaseURL() + "file/status/";
	private static final String URL_DOWNLOAD = GWT.getHostPageBaseURL() + "file/download/";

	private static final CssStyle STYLE_DRAGOVER = new SimpleStyle("file-dragover");
	private static final CssStyle STYLE_MUTTED = new SimpleStyle("text-muted");

	final Timer timer = new Timer() {
		@Override
		public void run() {
			InputFile.this.statusSendRequest();
		}
	};

	private class UploadForm {

		private final FormPanel formPanel = new FormPanel();
		private final FileUpload fileUpload = new FileUpload();

		private final HandlerRegistrationCollection handlerRegistrations =
			new HandlerRegistrationCollection();

		public UploadForm() {
			this.formPanel.setMethod("post");
			this.formPanel.setEncoding("multipart/form-data");
			this.formPanel.getElement().getStyle().setHeight(0, Unit.PX);
			this.formPanel.getElement().getStyle().setWidth(0, Unit.PX);
			this.formPanel.getElement().getStyle().setOverflow(Overflow.HIDDEN);
			this.formPanel.add(this.fileUpload);

			this.fileUpload.setName("data");

			this.handlerRegistrations.add(this.fileUpload.addChangeHandler(new ChangeHandler() {
				@Override
				public void onChange(ChangeEvent event) {
					UploadForm.this.handlerRegistrations.add(UploadForm.this.formPanel
						.addSubmitCompleteHandler(new SubmitCompleteHandler() {

							@Override
							public void onSubmitComplete(SubmitCompleteEvent event) {
								// Hook to strip <pre> on some brothers
								Element label = DOM.createLabel();
								label.setInnerHTML(event.getResults());
								InputFile.this.handleCompleteJson(label.getInnerText());
							}
						}));

					InputFile.this.fileId = UUID.uuid();
					UploadForm.this.formPanel.setAction(InputFile.URL_UPLOAD + InputFile.this.fileId);
					UploadForm.this.formPanel.submit();
					int fileSize = InputFile.getFileSize(UploadForm.this.fileUpload.getElement());
					InputFile.this.initProgressBar(fileSize);
				}
			}));
		}

		public UploadForm destroy() {
			InputElement.as(this.fileUpload.getElement()).setValue(null);
			this.handlerRegistrations.removeHandler();
			this.formPanel.removeFromParent();

			return null;
		}

		public void openFilePicker() {
			RootPanel.get().add(this.formPanel);
			InputFile.nativeClick(this.fileUpload.getElement());
		}
	}

	private final WidgetParams params = WidgetParams.Util.get();

	private final OutputProgressBar<Integer> progressBar = new OutputProgressBar<Integer>();
	private final OneWidgetPanel progressBarWrapper = new OneWidgetPanel();
	private final Anchor<?> fileNameAnchor = new Anchor();
	private final Text placeholderText = new Text();

	private final Button<?> cancelBtn = new Button();
	private final Button<?> uploadBtn = new Button();

	private String placeholder = null;

	private String fileId;
	private UploadForm uploadForm;
	private Request sendRequest;

	public InputFile() {
		this.endConstruct();
	}

	protected InputFile(InputFile source) {
		super(source, false);
		this.endConstruct();
	}

	private void endConstruct() {

		this.progressBar.setDisplayValue(true);
		this.progressBar.setAnimated(true);
		this.progressBarWrapper.add(this.progressBar);

		StyleUtils.addStyle(this.progressBarWrapper, AbstractInput.STYLE_CONTROL);
		StyleUtils.addStyle(this.fileNameAnchor, AbstractInput.STYLE_CONTROL);
		StyleUtils.addStyle(this.placeholderText, AbstractInput.STYLE_CONTROL);
		StyleUtils.addStyle(this.placeholderText, InputFile.STYLE_MUTTED);

		this.cancelBtn.setType(Type.ICON);
		this.cancelBtn.setIconType(IconFont.ICON_CANCEL);
		this.cancelBtn.addButtonHandler(new ButtonEvent.Handler() {
			@Override
			public void onButtonAction(ButtonEvent event) {
				if (InputFile.this.sendRequest != null) {
					InputFile.this.sendRequest.cancel();
				}
				InputFile.this.edit(null);
			}
		});
		this.uploadBtn.setType(Type.ICON);
		this.uploadBtn.setIconType(IconFont.ICON_UPLOAD);
		this.uploadBtn.addButtonHandler(new ButtonEvent.Handler() {
			@Override
			public void onButtonAction(ButtonEvent event) {
				if (InputFile.this.uploadForm == null) {
					InputFile.this.uploadForm = new UploadForm();
				}
				InputFile.this.uploadForm.openFilePicker();
			}
		});

		this.addDomHandler(new DragLeaveHandler() {
			@Override
			public void onDragLeave(DragLeaveEvent event) {
				StyleUtils.removeStyle(InputFile.this, InputFile.STYLE_DRAGOVER);
			}
		}, DragLeaveEvent.getType());
		this.addDomHandler(new DragOverHandler() {
			@Override
			public void onDragOver(DragOverEvent event) {
				StyleUtils.addStyle(InputFile.this, InputFile.STYLE_DRAGOVER);
			}
		}, DragOverEvent.getType());
		this.addDomHandler(new DropHandler() {
			@Override
			public void onDrop(DropEvent event) {
				event.preventDefault();
				event.stopPropagation();

				DataTransfer data = event.getNativeEvent().getDataTransfer();
				InputFile.nativeUploadData(data, InputFile.this);
			}
		}, DropEvent.getType());

		this.redraw();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputFile(this);
	}

	@Override
	public void redraw() {
		this.cancelBtn.removeFromParent();
		this.uploadBtn.removeFromParent();
		this.fileNameAnchor.removeFromParent();
		this.placeholderText.removeFromParent();
		this.progressBarWrapper.removeFromParent();
		if (this.fileId != null) {
			this.append(this.progressBarWrapper);
			this.addAddon(this.cancelBtn);
		} else {
			FileDto value = this.getValue();
			if (value != null) {
				NumberFormat nf = NumberFormat.getFormat("#.##");

				long size = value.getContentLength();
				String displaySize = "";
				if (size > 1024 * 1024) {
					displaySize = nf.format(size / (1024 * 1024D)) + " MB";
				} else if (size > 1024) {
					displaySize = nf.format(size / 1024D) + " KB";
				} else {
					displaySize = nf.format(size) + " B";
				}

				this.fileNameAnchor.setLink(InputFile.URL_DOWNLOAD + value.getToken());
				this.fileNameAnchor.setText(value.getName() + " - (" + displaySize + ")");
				this.placeholderText.setText(null);
				this.append(this.fileNameAnchor);
				this.addAddon(this.cancelBtn);
			} else {
				this.fileNameAnchor.setLink(null);
				this.fileNameAnchor.setText(null);
				this.placeholderText.setText(this.placeholder);
				this.append(this.placeholderText);
			}
			this.addAddon(this.uploadBtn);
		}
	}

	public String getPlaceholder() {
		return this.placeholder;
	}

	public void setPlaceholder(String placeholder) {
		this.placeholder = placeholder;
		this.placeholderText.setText(placeholder);
	}

	@Override
	public void edit(FileDto object) {
		super.edit(object);
		this.timer.cancel();
		this.redraw();
	}

	private void initProgressBar(int size) {
		this.progressBar.edit(0);
		this.redraw();
		if (this.params.inputFileProgressEnable()) {
			this.progressBar.setMax(size);
			this.progressBar.edit(0);
			this.timer.schedule(10);
		} else {
			this.progressBar.setDisplayValue(false);
			this.progressBar.setMax(100);
			this.progressBar.edit(40);
		}
	}

	private void statusSendRequest() {
		if (this.fileId == null) {
			return;
		}

		RequestCallback callback = new RequestCallback() {
			@Override
			public void onResponseReceived(Request request, Response response) {
				if (200 == response.getStatusCode()) {
					InputFile.this.handleStatusJson(response.getText());
				}
			}

			@Override
			public void onError(Request request, Throwable exception) {
			}
		};

		try {
			RequestBuilder requestBuilder =
				new RequestBuilder(RequestBuilder.GET, InputFile.URL_STATUS + this.fileId);
			requestBuilder.setHeader("Cache-Control", "max-age=0");
			requestBuilder.sendRequest("", callback);
		} catch (RequestException e) {
			throw new RuntimeException("Couldn't send request", e);
		}
	}

	private void uploadSendRequest(String base64data, String fileName, String type, int size) {
		RequestCallback callback = new RequestCallback() {
			@Override
			public void onResponseReceived(Request request, Response response) {
				if (200 == response.getStatusCode()) {
					InputFile.this.handleCompleteJson(response.getText());
				} else {
					InputFile.this.displayError("Couldn't retrieve JSON (" + response.getStatusText() + ")");
				}
			}

			@Override
			public void onError(Request request, Throwable exception) {
				InputFile.this.displayError("Couldn't retrieve JSON");
			}

		};

		this.fileId = UUID.uuid();
		this.initProgressBar(size);

		StringBuffer requestBody = new StringBuffer();
		requestBody.append("--").append(InputFile.MULTIPART_BOUNDARY).append(InputFile.EOL).append(
			"Content-Disposition: form-data; name=\"data\"; filename=\"").append(fileName).append("\"")
			.append(InputFile.EOL).append("Content-Type: ").append(type).append(InputFile.EOL).append(
				InputFile.EOL).append(base64data).append(InputFile.EOL).append("--").append(
				InputFile.MULTIPART_BOUNDARY).append("--");

		try {
			RequestBuilder requestBuilder =
				new RequestBuilder(RequestBuilder.POST, InputFile.URL_UPLOAD + this.fileId);
			requestBuilder.setHeader("content-type", "multipart/form-data; boundary="
				+ InputFile.MULTIPART_BOUNDARY);
			requestBuilder.setHeader("Cache-Control", "max-age=0");
			this.sendRequest = requestBuilder.sendRequest(requestBody.toString(), callback);
		} catch (RequestException e) {
			throw new RuntimeException("Couldn't send request", e);
		}
	}

	private void handleStatusJson(String reponseData) {

		if (!Strings.isNullOrEmpty(reponseData)) {
			JSONObject jsObject = JSONParser.parseLenient(reponseData).isObject();
			UploadStatus status = new UploadStatus();
			status.setBytesRead((long) jsObject.get("bytesRead").isNumber().doubleValue());
			status.setContentLength((long) jsObject.get("contentLength").isNumber().doubleValue());
			status.setUploadId(jsObject.get("uploadId").isString().stringValue());

			this.progressBar.setMax(Long.valueOf(status.getContentLength()).intValue());
			this.progressBar.edit((int) status.getBytesRead());
		} else {
			this.progressBar.setValue(this.progressBar.getMax());
		}
		if (this.fileId != null) {
			this.timer.schedule(100);
		}
	}

	private void handleCompleteJson(String reponseData) {
		JSONObject jsObject = JSONParser.parseLenient(reponseData).isObject();

		final FileDto file = new FileDto();
		file.setName(jsObject.get("name").isString().stringValue());
		file.setExtension(jsObject.get("extension").isString().stringValue());
		file.setMime(jsObject.get("mime").isString().stringValue());
		file.setToken(jsObject.get("token").isString().stringValue());
		file.setContentLength((long) jsObject.get("contentLength").isNumber().doubleValue());

		if (this.uploadForm != null) {
			this.uploadForm = this.uploadForm.destroy();
		}
		this.sendRequest = null;
		this.fileId = null;

		this.progressBar.edit(this.progressBar.getMax());

		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				InputFile.this.edit(file);
				return false;
			}
		}, this.params.inputFileProgressHideDelay());
	}

	private void displayError(String string) {
	}

	private static native void nativeUploadData(DataTransfer dataTransfer, InputFile inputFile)
	/*-{
		var files = dataTransfer.files;
		for (var i = 0; i < files.length; i++ ) {
			var file = files[i];
			var b64reader = new FileReader();
			b64reader.onloadend = (function(file) {
					return function(base64) {
						file.base64data = base64.target.result;
						inputFile.@fr.putnami.pwt.core.widget.client.InputFile::uploadSendRequest(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)
							(file.base64data, file.name, file.type, file.size);
					};
				})(file);
			b64reader.readAsBinaryString(file);
	   }
	}-*/;

	private static native int getFileSize(Element fileElement)
	/*-{
		var files = fileElement.files;
		for (var i = 0; i < files.length; i++ ) {
			return files[i].size;
		}
		return 0;
	}-*/;

	private static native void nativeClick(Element elem)
	/*-{
		elem.click();
	}-*/;

}
